#!/usr/bin/env bash
SHELL_DIR=$(
  cd "$(dirname $0)"
  pwd
)

engine_path="$1"
sdk_path="$2"
ndk_path="$3"

echo "engine_path: $engine_path"
echo "sdk_path: $sdk_path"
echo "ndk_path: $ndk_path"

cd "$engine_path"
pwd

if [ -f "$HOME/.bash_profile" ]; then
  source "$HOME/.bash_profile"
fi

export sdk_version=15.5.5.5 # engine sdk version, must be x.x.x.x
export android_sdk_root=$sdk_path
export android_ndk_root=$ndk_path
chmod 777 ./compile/configuration/setup_gn_args.py
export extra_args='\
is_debug=true \
trtc_sdk_major_version="11.4" \
trtc_sdk_build_number="13189" \
trtc_sdk_expected_version="11.4.13189" \
im_sdk_version="7.5.4852" \
im_sdk_expected_version="7.5.4852" \
use_trtc_release_sdk=true'
./compile/configuration/setup_gn_args.py --target-os android --sdk-version $sdk_version --extra-args="'$extra_args'" --android-sdk-root $android_sdk_root --android-ndk-root $android_ndk_root
gn gen out/room_engine_release_android
# 如果需要依赖指定版本IM/TRTC sdk，请自行覆盖 tuikit_engine/sdk/android目录下缓存sdk
ninja -C out/room_engine_release_android compile/devops:download_android_dependency_sdk
ninja -C out/room_engine_release_android copy_engine_room_sdk
