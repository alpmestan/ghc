%
% (c) The AQUA Project, Glasgow University, 1994-1999
%

\section[Char]{Module @Char@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Char 
    ( 
      Char

    , isAscii, isLatin1, isControl
    , isPrint, isSpace,  isUpper
    , isLower, isAlpha,  isDigit
    , isOctDigit, isHexDigit, isAlphaNum  -- :: Char -> Bool

    , toUpper, toLower  -- :: Char -> Char

    , digitToInt        -- :: Char -> Int
    , intToDigit        -- :: Int  -> Char

    , ord               -- :: Char -> Int
    , chr               -- :: Int  -> Char
    , readLitChar       -- :: ReadS Char 
    , showLitChar       -- :: Char -> ShowS
    , lexLitChar	-- :: ReadS String

    , String

     -- Implementation checked wrt. Haskell 98 lib report, 1/99.
    ) where

import PrelBase
import PrelRead (readLitChar, lexLitChar, digitToInt)
import {-# SOURCE #-} PrelErr   ( error )

\end{code}
