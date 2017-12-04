#ifndef __GHCPLATFORM_H__
#define __GHCPLATFORM_H__

#define BuildPlatform_TYPE  x86_64_apple_darwin
#define HostPlatform_TYPE   arm_unknown_linux

#define x86_64_apple_darwin_BUILD 1
#define arm_unknown_linux_HOST 1

#define x86_64_BUILD_ARCH 1
#define arm_HOST_ARCH 1
#define BUILD_ARCH "x86_64"
#define HOST_ARCH "arm"

#define darwin_BUILD_OS 1
#define linux_HOST_OS 1
#define BUILD_OS "darwin"
#define HOST_OS "linux"

#define apple_BUILD_VENDOR 1
#define unknown_HOST_VENDOR 1
#define BUILD_VENDOR "apple"
#define HOST_VENDOR "unknown"

/* These TARGET macros are for backwards compatibility... DO NOT USE! */
#define TargetPlatform_TYPE arm_unknown_linux
#define arm_unknown_linux_TARGET 1
#define arm_TARGET_ARCH 1
#define TARGET_ARCH "arm"
#define linux_TARGET_OS 1
#define TARGET_OS "linux"
#define unknown_TARGET_VENDOR 1

#endif /* __GHCPLATFORM_H__ */
