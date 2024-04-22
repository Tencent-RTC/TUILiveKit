# Live UIKit for iOS Quickstart

_English | [简体中文](README-zh_CN.md)_

<img src="https://qcloudimg.tencent-cloud.cn/raw/ec034fc6e4cf42cae579d32f5ab434a1.png" align="left" width=120 height=120>TUILiveKit is an interactive live streaming UI component launched by Tencent Cloud. By integrating this component, you can add interactive live streaming features to your app with just a few lines of code. It supports features such as interactive co-hosting, chat barrage, heartbeats, interactive gifts, and sound effects transformation.

## Before getting started

This section shows you the prerequisites you need for testing Live for iOS example.

### Requirements

- Xcode 13.0 or above
- Operating system: iOS 13.0 or later

## Getting started

If you would like to try the sample app specifically fit to your usage, you can do so by following the steps below.

### Create an application.

1. Go to the [Application management](https://console.cloud.tencent.com/trtc/app) page in the TRTC console, select **Create Application**, enter an application name such as `TUIKitDemo`, and click **Confirm**.
2. Click **Application Information** on the right of the application as shown below:
   <img src="https://qcloudimg.tencent-cloud.cn/raw/62f58d310dde3de2d765e9a460b8676a.png" width="900">
3. On the application information page, note the `SDKAppID` and key as shown below:
   <img src="https://qcloudimg.tencent-cloud.cn/raw/bea06852e22a33c77cb41d287cac25db.png" width="900">

> ! This feature uses two basic PaaS services of Tencent Cloud: [TRTC](https://www.tencentcloud.com/document/product/647/35078) and [IM](https://www.tencentcloud.com/document/product/1047/33513). When you activate TRTC, IM will be activated automatically. IM is a value-added service.

### Build and run the example

#### 1. Clone this repository

```
$ git clone git@github.com:tencentyun/TUILiveKit.git
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
