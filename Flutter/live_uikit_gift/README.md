# Live UIKit Gift

_English | [简体中文](README-zh_CN.md)_

<img src="https://qcloudimg.tencent-cloud.cn/raw/ec034fc6e4cf42cae579d32f5ab434a1.png" align="left" width=65 height=65>  
Live UIKit Gift is the gift component of [Interactive Live Streaming (tencent_live_uikit)](https://pub-web.flutter-io.cn/packages/tencent_live_uikit). By integrating this component, you can add interactive gift features to your live streaming with just a few lines of code.

## Features

- Supports sending and displaying bullet animations.
- Supports sending and displaying full-screen animations.
- Supports likes.

## Environment Preparation

### Flutter

- Flutter 3.27.4 or higher.
- Dart 3.6.2 or higher.

### Android

- Android Studio 3.5 or higher.
- Android devices with Android 5.0 or higher.

### iOS

- Xcode 15.0 or higher.
- Ensure your project has a valid developer signature.

## Getting Started

### Add Dependency

Follow the documentation to add the ```live_uikit_gift``` package as a [pubspec dependency](https://pub.flutter-io.cn/packages/live_uikit_gift/install).

### Activate Service

To use the gift feature of interactive live streaming, you need to ensure that the service is activated.

1. **Activate the Service**  
   You can activate the service and obtain ```SDKAppID``` and ```SDKSecretKey``` in the [Console](https://trtc.io/zh/document/60033?platform=flutter&product=live&menulabel=uikit).  
2. **Configure SDKAppID and SDKSecretKey**  
   Open the ```example/lib/debug/generate_test_user_sig.dart``` file and fill in the obtained ```SDKAppID``` and ```SDKSecretKey```:

   ```dart
   static int sdkAppId = 0; // Replace with your SDKAppID
   static String secretKey = ''; // Replace with your SDKSecretKey
   ```

## Example Experience

If you want to quickly integrate or experience the interactive gift effects, you can refer to the [example code](https://pub.dev/packages/live_uikit_gift/example) to integrate it into your application, or directly run the example program for a hands-on experience.

## Recommended Resources

If you want to quickly integrate interactive live streaming features into your application, you can integrate the [Interactive Live Streaming (tencent_live_uikit)](https://pub-web.flutter-io.cn/packages/tencent_live_uikit) component.

## Feedback and Support

If you have any needs or feedback, feel free to contact us at: <info_rtc@tencent.com>.
