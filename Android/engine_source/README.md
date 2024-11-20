## TUILiveKit 源码编译 engine 指南

注意：不要提交本模块到 github，仅供内部调试使用。

### engine_source 模块介绍

此模块依赖 engine 相关源码文件，并启动 engine sdk 的编译脚本。
需要在 build.gradle 配置 engine 源码路径 tuikitEnginePath。

### 源码编译配置

1、在 settings.gradle 添加 engine_source

```
include ':engine_source'
```

2、在 tuilivekit/build.gradle 添加 engine_source 依赖，同时注释旧的依赖。

```
      api project(':engine_source')
//    api rootProject.getProperties().containsKey("roomEngineSdk") ?
//            rootProject.ext.roomEngineSdk : 'io.trtc.uikit:rtc_room_engine:latest.release'
```
