_[中文](README-zh_CN.md) | English_

# Live UIKit for Android Quickstart

TUILiveKit  is a product suitable for interactive live scenes such as social entertainment, shopping, and fitness. By integrating this product, you can quickly add interactive multi-guest, send gift, room management, and other functions to your app in just three steps within 30 minutes.

## Before getting started

- Android 4.1 (SDK API level 16) or above; Android 5.0 (SDK API level 21) or above is recommended.
- Android Studio 3.5 or above
- Devices with Android 4.1 or above

## Getting started

## Step 1. Create application

1. Go to the [Application management](https://console.cloud.tencent.com/trtc/app) page in the TRTC console, select **Create Application**, enter an application name such as `TUIKitDemo`, and click **Confirm**.
2. Click **Application Information** on the right of the application as shown below:
   <img src="https://qcloudimg.tencent-cloud.cn/raw/62f58d310dde3de2d765e9a460b8676a.png" width="900">
3. On the application information page, note the `SDKAppID` and key as shown below:
   <img src="https://qcloudimg.tencent-cloud.cn/raw/bea06852e22a33c77cb41d287cac25db.png" width="900">

> ! This feature uses two basic PaaS services of Tencent Cloud: [TRTC](https://www.tencentcloud.com/document/product/647/35078) and [IM](https://www.tencentcloud.com/document/product/1047/33513). When you activate TRTC, IM will be activated automatically. IM is a value-added service.

### Step 2. Build and run the sample app
1. Clone thie repository
   git clone https://github.com/tencentyun/TUILiveKit.git
2. Open the demo project `TUILiveKit` with Android Studio (3.5 or above). And find and open the 
   `TUILiveKit/Android/debug/src/main/java/com/tencent/liteav/debug/GenerateTestUserSig.java`file.
3. Set parameters in `GenerateTestUserSig.java` as follows:
   <ul>
   <li>SDKAPPID: 0 by default. Replace it with your actual `SDKAPPID`.</li>
   <li>SECRETKEY: An empty string by default. Replace it with your actual `SECRETKEY`.</li>
   </ul>
4. You can open the source code project `TUILiveKit/Android` in Android Studio 3.5 or later, wait for the Android 
Studio project to be synced, connect to a real device, and click **Run** to try out the application.

### Step 3. Try out the application (**at least two devices required**)
1. Use two mobile phones (A and B) to log in to the application using strings as their user IDs. If you log in for the first time, you need to add a user name.
2. The user on mobile phone A clicks the 'Start Live Streaming' button to initiate a live broadcast (note that you select video live broadcast or voice live broadcast)
3. The user on mobile phone B enters the user ID of the user on mobile phone A and clicks to 'Join live room' or 'Join voice room' to watch the live broadcast.