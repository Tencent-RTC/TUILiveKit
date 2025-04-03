# Live UIKit Barrage

_English | [简体中文](README-zh_CN.md)_

<img src="https://qcloudimg.tencent-cloud.cn/raw/ec034fc6e4cf42cae579d32f5ab434a1.png" align="left"
width=65
height=65> Live UIKit Barrage is a barrage component of [Interactive Live Streaming (tencent_live_uikit)](https://pub-web.flutter-io.cn/packages/tencent_live_uikit). By integrating this component, you can add interactive barrage functionality to your live streaming with just a few lines of code.

## Features

- Supports sending and displaying regular text messages
- Supports sending and displaying emoji messages
- Supports custom barrage styles

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

### Install

Follow the documentation to add the ```live_uikit_barrage``` package as a [pubspec dependency](https://pub.flutter-io.cn/packages/live_uikit_barrage/install).

### Activate service

To use the barrage functionality in interactive live streaming, ensure you have activated the service.

1. **Activate the Service**
   You can activate the service and obtain the ```SDKAppID``` and ```SDKSecretKey``` in the [Console](https://trtc.io/zh/document/60033?platform=flutter&product=live&menulabel=uikit).
2. **Configure SDKAppID and SDKSecretKey**
   Open the ```example/lib/debug/generate_test_user_sig.dart``` file and fill in the obtained ```SDKAppID``` and ```SDKSecretKey```:

   ```dart
   static int sdkAppId = 0; // Replace with your SDKAppID
   static String secretKey = ''; // Replace with your SDKSecretKey
   ```

## Example Experience

If you want to quickly integrate or experience the interactive barrage effect, you can refer to the [Example](https://pub.dev/packages/live_uikit_barrage/example) to integrate it into your application, or directly run the example program to experience it.

## Recommended Resources

If you want to quickly integrate interactive live streaming functionality into your application, you can integrate the [Interactive Live Streaming (tencent_live_uikit)](https://pub-web.flutter-io.cn/packages/tencent_live_uikit) component.

## Feedback and Support

If you have any needs or feedback, you can contact: <info_rtc@tencent.com>.
