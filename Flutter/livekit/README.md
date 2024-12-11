# Live UIKit for Flutter Quickstart

_English | [简体中文](README-zh_CN.md)_

<img src="https://qcloudimg.tencent-cloud.cn/raw/ec034fc6e4cf42cae579d32f5ab434a1.png" align="left" width=65 height=65>TUILiveKit is an interactive live streaming UI component launched by Tencent Cloud. By integrating this component, you can add interactive live streaming features to your app with just a few lines of code. It supports features such as interactive co-hosting, chat barrage, heartbeats, interactive gifts, and sound effects transformation.

## Before getting started

This section shows you the prerequisites you need for testing Live for Flutter example.

### Requirements

### Flutter
- Flutter 3.22.0 or later.
- Dart version 3.4.0 or higher.

### Android
- Android Studio 3.5 or later.
- Android devices 5.0 or later.

### iOS
- Xcode 13.0 or later.
- Please ensure that your project has a valid developer signature set.

## Getting started

If you would like to try the sample app specifically fit to your usage, you can do so by following the steps below.

### Create an application.

1. Go to the [Application management](https://console.trtc.io/app) page in the TRTC console, select **Create Application**, enter an application name such as `TUIKitDemo`, and click **Confirm**.
2. Click **Application Information** on the right of the application, note the `SDKAppID` and key:
   - SDKAppID: A number in parentheses after 'TUIKitDemo'.
   - SDKSecretKey: Click **Copy SDKSecretKey**.

### Build and run the example

#### 1. Clone this repository

```
git clone https://github.com/Tencent-RTC/TUILiveKit.git
```

#### 2. Open the TUILiveKit/Flutter project through Android Studio

#### 3. Set SDKAppID and SDKSecretKey

Open the Flutter/example/lib/debug/generate_test_user_sig.dart files, will be submitted to apply for access to the corresponding SDKAppID and SDKSecretKey fill in among them:

```
static int sdkAppId = 0;
static String secretKey = '';
```

#### 4. Build and run the example application on Simulator or iOS devices

## Making your first live

1. Use two mobile phones (A and B) to log in to the application using a string of user ids. If you log in to the application for the first time, you need to add a user name
2. The user on mobile phone A clicks the "Start Live" button to initiate live broadcast
3. You can pull down to refresh the room list on mobile phone B, and click to enter the live broadcast room to watch the live broadcast
