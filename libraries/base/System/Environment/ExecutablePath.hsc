{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Environment.ExecutablePath
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Function to retrieve the absolute filepath of the current executable.
--
-- @since 4.6.0.0
-----------------------------------------------------------------------------

module System.Environment.ExecutablePath ( getExecutablePath ) where

-- The imports are purposely kept completely disjoint to prevent edits
-- to one OS implementation from breaking another.

#if defined(darwin_HOST_OS)
import Data.Word
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.Posix.Internals
#elif defined(linux_HOST_OS)
import Foreign.C
import Foreign.Marshal.Array
import System.Posix.Internals
#elif defined(mingw32_HOST_OS)
import Data.Word
import Foreign.C
import Foreign.Marshal.Array
import Foreign.Ptr
import System.Posix.Internals
import System.Win32.DLL
import System.Win32.File
import System.Win32.Types
#else
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.Posix.Internals
#endif

-- The exported function is defined outside any if-guard to make sure
-- every OS implements it with the same type.

-- | Returns the absolute pathname of the current executable.
--
-- Note that for scripts and interactive sessions, this is the path to
-- the interpreter (e.g. ghci.)
--
-- @since 4.6.0.0
getExecutablePath :: IO FilePath

--------------------------------------------------------------------------------
-- Mac OS X

#if defined(darwin_HOST_OS)

type UInt32 = Word32

foreign import ccall unsafe "mach-o/dyld.h _NSGetExecutablePath"
    c__NSGetExecutablePath :: CString -> Ptr UInt32 -> IO CInt

-- | Returns the path of the main executable. The path may be a
-- symbolic link and not the real file.
--
-- See dyld(3)
_NSGetExecutablePath :: IO FilePath
_NSGetExecutablePath =
    allocaBytes 1024 $ \ buf ->  -- PATH_MAX is 1024 on OS X
    alloca $ \ bufsize -> do
        poke bufsize 1024
        status <- c__NSGetExecutablePath buf bufsize
        if status == 0
            then peekFilePath buf
            else do reqBufsize <- fromIntegral `fmap` peek bufsize
                    allocaBytes reqBufsize $ \ newBuf -> do
                        status2 <- c__NSGetExecutablePath newBuf bufsize
                        if status2 == 0
                             then peekFilePath newBuf
                             else errorWithoutStackTrace "_NSGetExecutablePath: buffer too small"

foreign import ccall unsafe "stdlib.h realpath"
    c_realpath :: CString -> CString -> IO CString

-- | Resolves all symbolic links, extra \/ characters, and references
-- to \/.\/ and \/..\/. Returns an absolute pathname.
--
-- See realpath(3)
realpath :: FilePath -> IO FilePath
realpath path =
    withFilePath path $ \ fileName ->
    allocaBytes 1024 $ \ resolvedName -> do
        _ <- throwErrnoIfNull "realpath" $ c_realpath fileName resolvedName
        peekFilePath resolvedName

getExecutablePath = _NSGetExecutablePath >>= realpath

--------------------------------------------------------------------------------
-- Linux

#elif defined(linux_HOST_OS)

foreign import ccall unsafe "readlink"
    c_readlink :: CString -> CString -> CSize -> IO CInt

-- | Reads the @FilePath@ pointed to by the symbolic link and returns
-- it.
--
-- See readlink(2)
readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink file =
    allocaArray0 4096 $ \buf -> do
        withFilePath file $ \s -> do
            len <- throwErrnoPathIfMinus1 "readSymbolicLink" file $
                   c_readlink s buf 4096
            peekFilePathLen (buf,fromIntegral len)

getExecutablePath = readSymbolicLink $ "/proc/self/exe"

--------------------------------------------------------------------------------
-- Windows

#elif defined(mingw32_HOST_OS)

# if defined(i386_HOST_ARCH)
##  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
##  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif

foreign import WINDOWS_CCONV unsafe "windows.h GetModuleFileNameW"
    c_GetModuleFileName :: Ptr () -> CWString -> Word32 -> IO Word32

getExecutablePath = go 2048  -- plenty, PATH_MAX is 512 under Win32
  where
    go size = allocaArray (fromIntegral size) $ \ buf -> do
        ret <- c_GetModuleFileName nullPtr buf size
        case ret of
            0 -> errorWithoutStackTrace "getExecutablePath: GetModuleFileNameW returned an error"
            _ | ret < size -> do
                  path <- peekFilePath buf
                  sanitize <$> getFinalPath path
              | otherwise  -> go (size * 2)

    sanitize s = if "\\\\?\\" `isPrefixOf` s
                    then drop 4 s
                    else s

-- Attempt to resolve symlinks in order to find the actual location GHC
-- is located at. See Trac #11759.
getFinalPath :: FilePath -> IO (Maybe FilePath)
getFinalPath name = do
    dllHwnd <- failIfNull "LoadLibrary"     $ loadLibrary "kernel32.dll"
    -- Note: The API GetFinalPathNameByHandleW is only available starting from Windows Vista.
    -- This means that we can't bind directly to it since it may be missing.
    -- Instead try to find it's address at runtime and if we don't succeed consider the
    -- function failed.
    addr_m  <- (fmap Just $ failIfNull "getProcAddress" $ getProcAddress dllHwnd "GetFinalPathNameByHandleW")
                  `catch` (\(_ :: SomeException) -> return Nothing)
    case addr_m of
      Nothing   -> return Nothing
      Just addr -> do handle  <- failIf (==iNVALID_HANDLE_VALUE) "CreateFile"
                                        $ createFile name
                                                     gENERIC_READ
                                                     fILE_SHARE_READ
                                                     Nothing
                                                     oPEN_EXISTING
                                                     (fILE_ATTRIBUTE_NORMAL .|. fILE_FLAG_BACKUP_SEMANTICS)
                                                     Nothing
                      let fnPtr = makeGetFinalPathNameByHandle $ castPtrToFunPtr addr
                      -- First try to resolve the path to get the actual path
                      -- of any symlinks or other file system redirections that
                      -- may be in place. However this function can fail, and in
                      -- the event it does fail, we need to try using the
                      -- original path and see if we can decompose that.
                      -- If the call fails Win32.try will raise an exception
                      -- that needs to be caught. See #14159
                      path    <- (Win32.try "GetFinalPathName"
                                    (\buf len -> fnPtr handle buf len 0) 512
                                    `finally` closeHandle handle)
                                `catch`
                                 (\(_ :: IOException) -> return name)
                      return $ Just path

type GetFinalPath = HANDLE -> LPTSTR -> DWORD -> DWORD -> IO DWORD

foreign import WINDOWS_CCONV unsafe "dynamic"
  makeGetFinalPathNameByHandle :: FunPtr GetFinalPath -> GetFinalPath

--------------------------------------------------------------------------------
-- Fallback to argv[0]

#else

foreign import ccall unsafe "getFullProgArgv"
    c_getFullProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

getExecutablePath =
    alloca $ \ p_argc ->
    alloca $ \ p_argv -> do
        c_getFullProgArgv p_argc p_argv
        argc <- peek p_argc
        if argc > 0
            -- If argc > 0 then argv[0] is guaranteed by the standard
            -- to be a pointer to a null-terminated string.
            then peek p_argv >>= peek >>= peekFilePath
            else errorWithoutStackTrace $ "getExecutablePath: " ++ msg
  where msg = "no OS specific implementation and program name couldn't be " ++
              "found in argv"

--------------------------------------------------------------------------------

#endif
