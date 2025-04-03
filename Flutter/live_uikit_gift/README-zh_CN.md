# Live UIKit Gift

_[English](README.md) | 简体中文_

<img src="https://qcloudimg.tencent-cloud.cn/raw/ec034fc6e4cf42cae579d32f5ab434a1.png" align="left"
width=65
height=65> Live UIKit Gift 是 [互动直播(tencent_live_uikit)](https://pub-web.flutter-io.cn/packages/tencent_live_uikit) 的礼物组件，通过集成该组件，您只需要编写几行代码就可以为您的互动直播添加互动礼物功能。

## 产品特性

- 支持发送与显示子弹动画
- 支持发送与显示全屏动画
- 支持点赞

## 环境准备

### Flutter

- Flutter 3.27.4 或更高版本。
- Dart 3.6.2 或更高的版本。

### Android

- Android Studio 3.5及以上版本。
- Android 5.0 及以上版本的 Android 设备。

### iOS

- Xcode 15.0及以上版本。
- 请确保您的项目已设置有效的开发者签名。

## 开始使用

### 添加依赖

依照文档将 ```live_uikit_gift``` 包添加为 [pubspec 的依赖](https://pub.flutter-io.cn/packages/live_uikit_gift/install)。

### 激活服务

为使用互动直播的礼物功能，您需要确保您已激活服务。

1. **开通服务**
   您可在 [控制台](https://trtc.io/zh/document/60033?platform=flutter&product=live&menulabel=uikit) 开通服务并获取 ```SDKAppID``` 和 ```SDKSecretKey``` 。
2. **配置 SDKAppID 和 SDKSecretKey**
   打开 ```example/lib/debug/generate_test_user_sig.dart``` 文件，将获取到的 ```SDKAppID``` 和 ```SDKSecretKey``` 填入其中：

    ```dart
    static int sdkAppId = 0; // 替换成您已开通应用的SDKAppID
    static String secretKey = ''; // 替换成您已开通应用的SDKSecretKey
    ```
  
## 示例体验

若您想快速集成或体验互动礼物效果，您可以参考 [示例代码](https://pub.dev/packages/live_uikit_gift/example) 将其集成到您的应用中，或直接运行 exmaple 程序进行体验。

## 推荐资源

如果您想快速为您的应用集成互动直播的功能，您可集成 [互动直播(tencent_live_uikit)](https://pub-web.flutter-io.cn/packages/tencent_live_uikit) 组件。

## 交流反馈

如果有任何需要或者反馈，您可以联系：<info_rtc@tencent.com>。
