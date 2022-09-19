# Quick Run of TUILiveRoom Demo for Android

_[中文](README.md) | English_

This document describes how to quickly run the TUILiveRoom demo project to try out high-quality audio interaction. For more information on the TUILiveRoom component connection process, see **[Integrating TUILiveRoom (Android)](https://cloud.tencent.com/document/product/647/43182)**.

## Directory Structure

```
TUILiveRoom
├─ app             // Main panel, which provides the scenario entry
├─ debug           // Debugging code
├─ tuibeauty       // Beauty filter panel, which provides effects such as beauty filters, filters, and animated effects
├─ tuigift         // Core Gift component
├─ tuiaudioeffect  // Core Audio Effect component
├─ tuibarrage      // Core Barrage component
└─ tuiliveroom     // Interactive live streaming business logic
```

## Environment Requirements

- Android 4.1 (SDK API level 16) or above; Android 5.0 (SDK API level 21) or above is recommended.
- Android Studio 3.5 or above
- Devices with Android 4.1 or above

## Demo Run Example

## Step 1. Create a TRTC application

1. Go to the [Application management](https://console.cloud.tencent.com/trtc/app) page in the TRTC console, select **Create Application**, enter an application name such as `TUIKitDemo`, and click **Confirm**.
2. Click **Application Information** on the right of the application as shown below:
   <img src="https://qcloudimg.tencent-cloud.cn/raw/62f58d310dde3de2d765e9a460b8676a.png" width="900">
3. On the application information page, note the `SDKAppID` and key as shown below:
   <img src="https://qcloudimg.tencent-cloud.cn/raw/bea06852e22a33c77cb41d287cac25db.png" width="900">

> ! This feature uses two basic PaaS services of Tencent Cloud: [TRTC](https://cloud.tencent.com/document/product/647/16788) and [IM](https://cloud.tencent.com/document/product/269). When you activate TRTC, IM will be activated automatically. IM is a value-added service. See [Pricing](https://cloud.tencent.com/document/product/269/11673) for its billing details.

### Step 2. Download the source code and configure the project

1. Open the demo project `TUILiveRoom` with Android Studio (3.5 or above).
2. Find and open the `TUILiveRoom/Android/debug/src/main/java/com/tencent/liteav/debug/GenerateTestUserSig.java`file.
3. Set parameters in `GenerateTestUserSig.java` as follows:

<ul>
<li>SDKAPPID: 0 by default. Replace it with your actual `SDKAPPID`.</li>
<li>SECRETKEY: An empty string by default. Replace it with your actual `SECRETKEY`.</li>
<li>XMAGIC_LICENSE_URL【Optional】: An empty string by default. Please vist: <a href="https://cloud.tencent.com/document/product/616/65878">XMagic License</a>。</li>
<li>XMAGIC_LICENSE_KEY【Optional】: An empty string by default. Please vist: <a href="https://cloud.tencent.com/document/product/616/65878">XMagic License</a>。</li>
</ul>
<img src="https://liteav.sdk.qcloud.com/doc/res/trtc/picture/zh-cn/sdkappid_secretkey.png">

### Step 3. Compile and run the application

You can open the source code project `TUILiveRoom/Android` in Android Studio 3.5 or later, wait for the Android Studio project to be synced, connect to a real device, and click **Run** to try out the application.

### Step 4. Try out the application (**at least two devices required**)

**Device A (userId: 111)**

- Step 1: On the welcome page, enter the username (which must be unique), such as `111`.
- Step 2: Click **Create Room**.
- Step 3: Enter a room subject and click **Start**.
- Step 4: After successful creation, you will enter the main UI of video live streaming. Note down the room number at this time.

| Step 1 | Step 2 | Step 3 | Step 4 |
|---------|---------|---------|---------|
| <img src="https://qcloudimg.tencent-cloud.cn/raw/24a76a18049eda3bdb6414493d43e286.png" width="250"> | <img src="https://qcloudimg.tencent-cloud.cn/raw/8f9290c8dfc3eaa44f3c0a82e776c497.png" width="250"> | <img src="https://qcloudimg.tencent-cloud.cn/raw/4ac45382e20f72a87b72104404eee2da.png" width="250"> |<img src="https://qcloudimg.tencent-cloud.cn/raw/01c73737f0af40fe17c70e1107a9f720.jpeg" width="250"> |

**Device B (userId: 222)**

- Step 1: Enter the username (which must be unique), such as `222`.
- Step 2: Click **Enter Room** and enter the ID of the room created by user A (the room ID that you noted down in step 4 on device A).

| Step 1 | Step 2 | 
|---------|---------|
| <img src="https://liteav.sdk.qcloud.com/doc/res/trtc/picture/zh-cn/user_b_ios.png" width="320"/> | <img src="https://qcloudimg.tencent-cloud.cn/raw/fe39e76723f304de52b9d677a8cebf97.png" width="320"/> | 

## Have any questions?
Welcome to join our Telegram Group to communicate with our professional engineers! We are more than happy to hear from you~
Click to join: https://t.me/+EPk6TMZEZMM5OGY1
Or scan the QR code

<img src="https://qcloudimg.tencent-cloud.cn/raw/9c67ed5746575e256b81ce5a60216c5a.jpg" width="320"/>
