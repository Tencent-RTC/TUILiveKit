_[简体中文](README-zh_CN.md) | English_

# Live UIKit for Android Quickstart

<img src="https://qcloudimg.tencent-cloud.cn/raw/ec034fc6e4cf42cae579d32f5ab434a1.png" align="left" width=65 height=65>TUILiveKit is an interactive live streaming UI component launched by Tencent Cloud. By integrating this component, you can add interactive live streaming features to your app with just a few lines of code. It supports features such as interactive co-hosting, chat barrage, heartbeats, interactive gifts, and sound effects transformation.

## Before getting started

- Android 5.0 (SDK API level 21) or above.
- Android Studio 4.2.1 or above
- Devices with Android 5.0 or above

## Getting started

## Step 1. Create application

1. Go to the [Application management](https://console.trtc.io/app) page in the TRTC console, select **Create Application**, enter an application name such as `TUIKitDemo`, and click **Confirm**.
2. Click **Application Information** on the right of the application, note the `SDKAppID` and key:
   - SDKAppID: A number in parentheses after 'TUIKitDemo'.
   - SDKSecretKey: Click **Copy SDKSecretKey**.

### Step 2. Build and run the sample app
1. Clone this repository
   git clone https://github.com/Tencent-RTC/TUILiveKit.git
2. Open the demo project `TUILiveKit` with Android Studio (3.5 or above). And find and open the 
   `TUILiveKit/Android/debug/src/main/java/com/tencent/qcloud/tuikit/debug/GenerateTestUserSig.java`file.
3. Set parameters in `GenerateTestUserSig.java` as follows:
   <ul>
   <li>SDKAPPID: 0 by default. Replace it with your actual `SDKAPPID`.</li>
   <li>SECRETKEY: An empty string by default. Replace it with your actual `SECRETKEY`.</li>
   </ul>
4. You can open the source code project `TUILiveKit/Android` in Android Studio 3.5 or later, wait for the Android 
Studio project to be synced, connect to a real device, and click **Run** to try out the application.

### Step 3. Try out the application (**at least two devices required**)
1. Use two mobile phones (A and B) to log in to the application using strings as their user IDs. If you log in for the first time, you need to add a user name.
2. The user on mobile phone A clicks the 'GO LIVE' button to initiate a live broadcast (note that you select video live broadcast or voice live broadcast)
3. The user on mobile phone B enters the user ID of the user on mobile phone A and clicks to 'Join live room' or 'Join voice room' to watch the live broadcast.


## FAQs

Please refer to: [FAQs](https://www.tencentcloud.com/document/product/647/60043?lang=en&pg=)

#### 1.Can TUILiveKit use TRTC without introducing IM SDK?
```
No, all the components of TUIKit use Tencent Cloud IM SDK as the basic service for communication,
such as the core logic of creating room signaling, Lian-mic signaling, etc., all use IM services. 
If you have purchased other IM products, you can also refer to TUILiveKit logic to adapt.
```

#### 2."application@allowBackup", Error details:
```
Manifest merger failed : Attribute application@allowBackup value=(false) from AndroidManifest.xml:7:9-36
	is also present at [com.github.yyued:SVGAPlayer-Android:2.6.1] AndroidManifest.xml:12:9-35 value=(true).
	Suggestion: add 'tools:replace="android:allowBackup"' to <application> element at AndroidManifest.xml:5:5-53:19 to override.
```

#### 3. Activity need to use a Theme.AppCompat theme, Error details:
```
java.lang.RuntimeException: Unable to start activity ComponentInfo{com.trtc.uikit.livekit.example/com.trtc.uikit.livekit.example.login.LoginActivity}: 
java.lang.IllegalStateException: You need to use a Theme.AppCompat theme (or descendant) with this activity.
	at android.app.ActivityThread.performLaunchActivity(ActivityThread.java:3730)
	at android.app.ActivityThread.handleLaunchActivity(ActivityThread.java:3885)
	at android.app.servertransaction.LaunchActivityItem.execute(LaunchActivityItem.java:101)
	at android.app.servertransaction.TransactionExecutor.executeCallbacks(TransactionExecutor.java:135)
	at android.app.servertransaction.TransactionExecutor.execute(TransactionExecutor.java:95)
	at android.app.ActivityThread$H.handleMessage(ActivityThread.java:2332)
	at android.os.Handler.dispatchMessage(Handler.java:107)
	at android.os.Looper.loop(Looper.java:230)
	at android.app.ActivityThread.main(ActivityThread.java:8115)
	at java.lang.reflect.Method.invoke(Native Method)
	at com.android.internal.os.RuntimeInit$MethodAndArgsCaller.run(RuntimeInit.java:526)
	at com.android.internal.os.ZygoteInit.main(ZygoteInit.java:1034)
```

#### 4.Failed to open the web page address in the browser, Error details:
```
No activity match : Intent { act=android.intent.action.VIEW dat=https://cloud.tencent.com/... }
```