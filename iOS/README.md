# Live UIKit for iOS Quickstart

_English | [简体中文](README-zh_CN.md)_

<img src="https://qcloudimg.tencent-cloud.cn/raw/ec034fc6e4cf42cae579d32f5ab434a1.png" align="left" width=65 height=65>TUILiveKit is an interactive live streaming UI component launched by Tencent Cloud. By integrating this component, you can add interactive live streaming features to your app with just a few lines of code. It supports features such as interactive co-hosting, chat barrage, heartbeats, interactive gifts, and sound effects transformation.

## Before getting started

This section shows you the prerequisites you need for testing Live for iOS example.

### Requirements

- Xcode 13.0 or above
- Operating system: iOS 13.0 or later

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
$ git clone https://github.com/Tencent-RTC/TUILiveKit.git
```

#### 2. Install dependencies

```
$ cd TUILiveKit/iOS/Example
$ pod install
```

#### 3. Specify the SDKAppID and SDKSecretKey

In `GenerateTestUserSig.swift`, you need to configure the application's `SDKAppId` and `SDKSecretKey`.

```
let SDKAPPID: Int = 0
let SECRETKEY = ""
```

#### 4. Build and run the example application on Simulator or iOS devices

## Making your first live

1. Use two mobile phones (A and B) to log in to the application using strings as their user IDs. If you log in for the first time, you need to add a user name.
2. The user on mobile phone A clicks the 'Start Live Streaming' button to initiate a live broadcast (note that you select video live broadcast or voice live broadcast)
3. The user on mobile phone B enters the user ID of the user on mobile phone A and clicks to 'Join live room' or 'Join voice room' to watch the live broadcast.

## FAQs
#### “Sandbox: rsync”，Error details:

```
Sandbox: rsync.samba(2564) deny(1) file-write-unlink /Users/wesleylei/Library/Developer/Xcode/DerivedData/TestLiveKit-etglzzsjcwgokmcvmmnjifiqfgfx/Build/Products/Debug-iphoneos/TestLiveKit.app/Frameworks/Kingfisher.framework/_CodeSignature

```

#### “SDK does not contain”，Error details:
```
clang: error: SDK does not contain 'libarclite' at the path '/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/arc/libarclite_iphoneos.a'; try increasing the minimum deployment target
```
#### “Linker command failed with exit code 1 (use -v to see invocation)”，Error details:

```
ld: Undefined symbols:
  _OBJC_CLASS_$_SDImageCoderHelper, referenced from:
       in TUITool.o
clang: error: linker command failed with exit code 1 (use -v to see invocation)
```
Please refer to：[FAQs](https://www.tencentcloud.com/zh/document/product/647/60048?lang=zh&pg=)
