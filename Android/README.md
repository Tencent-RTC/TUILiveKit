_[简体中文](README-zh_CN.md) | English_

# Live UIKit for Android Quickstart

<img src="https://qcloudimg.tencent-cloud.cn/raw/ec034fc6e4cf42cae579d32f5ab434a1.png" align="left" width=65 height=65>TUILiveKit is an interactive live streaming UI component launched by Tencent Cloud. By integrating this component, you can add interactive live streaming features to your app with just a few lines of code. It supports features such as interactive co-hosting, chat barrage, heartbeats, interactive gifts, and sound effects transformation.

## Before getting started

- Android 4.1 (SDK API level 16) or above; Android 5.0 (SDK API level 21) or above is recommended.
- Android Studio 3.5 or above
- Devices with Android 4.1 or above

## Getting started

## Step 1. Create application

1. Go to the [Application management](https://console.trtc.io/app) page in the TRTC console, select **Create Application**, enter an application name such as `TUIKitDemo`, and click **Confirm**.
2. Click **Application Information** on the right of the application, note the `SDKAppID` and key:
   - SDKAppID: A number in parentheses after 'TUIKitDemo'.
   - SDKSecretKey: Click **Copy SDKSecretKey**.

### Step 2. Build and run the sample app
1. Clone thie repository
   git clone git@github.com:Tencent-RTC/TUILiveKit.git
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
