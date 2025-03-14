_简体中文 | [English](README.md)_

# Live UIKit 示例工程快速跑通

<img src="https://qcloudimg.tencent-cloud.cn/raw/ec034fc6e4cf42cae579d32f5ab434a1.png" align="left" width=65 height=65>TUILiveKit 是腾讯云推出一款互动直播的含 UI 组件，通过集成该组件，您只需要编写几行代码就可以为您的 App 添加互动直播功能，并且支持互动连麦、聊天弹幕、心动点赞、互动礼物以及音效变声等功能。

## 环境准备

- Android 5.0（SDK API Level 21）及以上版本。
- Gradle 4.2.1 及以上的版本。
- Android 5.0 及以上的手机设备。

## 跑通示例

## 第一步：创建应用

1. 一键进入腾讯云实时音视频控制台的[应用管理](https://console.trtc.io/app)界面，选择创建应用，输入应用名称，例如 `TUIKitDemo`，单击 **创建**；
2. 点击对应应用条目后**应用信息**，记录 SDKAppID 和密钥：
   - SDKAppID：`TUIKitDemo`后括号里的一串数字。
   - SDKSecretKey：点击**复制SDKSecretKey**即可。
### 第二步：构建并运行应用程序
1. 下载源码：
   git clone https://github.com/Tencent-RTC/TUILiveKit.git
2. 使用 Android Studio（3.5及以上的版本）打开源码工程`TUILiveKit` 并找到并打开`TUILiveKit/Android/debug/src/main/java/com/tencent/qcloud/tuikit/debug/GenerateTestUserSig.java`文件。
3. 设置`GenerateTestUserSig.java`文件中的相关参数：
   <ul>
   <li>SDKAPPID：默认为 0 ，请设置为实际申请的SDKAPPID。</li>
   <li>SECRETKEY：默认为空字符串，请设置为实际申请的SECRETKEY。</li>
   </ul>
4. 使用 Android Studio（3.5及以上的版本）打开源码工程`TUILiveKit/Android `，等待Android Studio工程同步完成后，连接真机点击运行按钮即可体验本APP

### 第三步：体验应用（**体验应用至少需要两台设备**）
1. 使用两台手机（A、B）用字符串作为自己的用户ID登录应用,如果第一次登录需要添加用户名
2. 手机A上的用户点击开始直播按钮发起直播（注意选择视频直播或者语音直播）
3. 手机B上的用户输入手机A上用户的用户ID，点击加入直播或者加入语聊房观看直播

## 常见问题

请参考：[常见问题](https://trtc.io/zh/document/60043?platform=android&product=live&menulabel=uikit)

#### 1.TUILiveKit 是否可以不引入 IM SDK，只使用 TRTC？
```
不可以，TUIKit 全系组件都使用了腾讯云 IM SDK 作为通信的基础服务，例如创建房间信令、连麦信令等核心逻辑都使用 IM服务，
如果您已经购买有其他 IM 产品，也可以参照  TUILiveKit 逻辑进行适配。
```

#### 2."application@allowBackup"，错误详情：
```
Manifest merger failed : Attribute application@allowBackup value=(false) from AndroidManifest.xml:7:9-36
	is also present at [com.github.yyued:SVGAPlayer-Android:2.6.1] AndroidManifest.xml:12:9-35 value=(true).
	Suggestion: add 'tools:replace="android:allowBackup"' to <application> element at AndroidManifest.xml:5:5-53:19 to override.
```

#### 3.Activity need to use a Theme.AppCompat theme，错误详情：
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

#### 4. 跳转浏览器打开网页地址失败，错误详情：
```
No activity match : Intent { act=android.intent.action.VIEW dat=https://cloud.tencent.com/... }
```