#!/usr/bin/env bash

# source: https://stackoverflow.com/a/27776822/700597
case "$(uname -s)" in

   Darwin)
     WINDOWS_SOCKET_PREFIX=""
     ;;

   Linux)
     WINDOWS_SOCKET_PREFIX=""
     ;;

   CYGWIN*|MINGW32*|MSYS*|MINGW*)
     WINDOWS_SOCKET_PREFIX='\\.\pipe\'
     ;;

   *)
     echo "$0: failed to detect OS. uname -s: $(uname -s)"
     ;;
esac