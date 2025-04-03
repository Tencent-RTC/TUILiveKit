# TUILiveKit Release Notes
# 2025.03.31
## Version 3.0

### RoomEngine
#### New Features
- Added fuzzy member search support for live streaming rooms
- Added custom layout interface in LayoutManager
- Added screen sharing pause interface
- Completed live list interface for Web
- Added room metadata interface for Web

#### API Changes
- Error code updates
- Added mute field to userInfo parameter in OnRemoteUserEnterRoom/OnRemoteUserLeaveRoom
- Swift API declaration changes for mute APIs on iOS

#### Bug Fixes
- Optimized screen sharing exceptions
- Fixed occasional crashes on Android
- Fixed potential microphone activation failure after entering room

### LiveStreamCore
#### Flutter
- New live streaming & voice chat components

#### iOS & Android
- Static library release
- State status now accessible and subscribable
- Added switchCamera & enableMirror interfaces
- Unified internationalization copywriting
- Fixed known issues

### TUILiveKit
#### Flutter
- New voice chat room scenario
- Unified internationalization copywriting
- Fixed known issues

#### iOS & Android
- Fixed authentication issues with advanced effects player
- iOS advanced beauty effects now support historical presets
- Added live room user management (mute, kick, remove from stage, AV ban, etc.)
- Removed incomplete features (user level, music playback, live streaming types)
- Fixed abnormal room behavior caused by frequent gifting
- Unified internationalization copywriting
- iOS live list information now supports customization
- Optimized compatibility issues with specific Android models
- Audience automatically leaves stage when entering room via co-streaming
- Fixed known issues
