_中文 | [English](README.md)_

# Live UIKit 示例工程快速跑通

TUILiveKit 是一款适用于社交娱乐、购物、健身等互动直播场景的产品，通过集成该产品，仅需三步，30分钟内就可以为您的 App 添加互动连麦、送礼、房间管理等功能。

## 环境准备

- 最低兼容 Android 4.1（SDK API Level 16），建议使用 Android 5.0 （SDK API Level 21）及以上版本
- Android Studio 3.5及以上版本
- App 要求 Android 4.1及以上设备

## 运行示例

## 第一步：创建应用

1. 一键进入腾讯云实时音视频控制台的[应用管理](https://console.cloud.tencent.com/trtc/app)界面，选择创建应用，输入应用名称，例如 `TUIKitDemo`，单击 **创建**；
2. 点击对应应用条目后**应用信息**，具体位置如下下图所示：
   <img src="https://qcloudimg.tencent-cloud.cn/raw/62f58d310dde3de2d765e9a460b8676a.png" width="900">
3. 进入应用信息后，按下图操作，记录SDKAppID和密钥：
   <img src="https://qcloudimg.tencent-cloud.cn/raw/bea06852e22a33c77cb41d287cac25db.png" width="900">

> ! 本功能同时使用了腾讯云 [实时音视频 TRTC](https://cloud.tencent.com/document/product/647/16788) 和 [即时通信 IM](https://cloud.tencent.com/document/product/269) 两个基础 PaaS 服务，开通实时音视频后会同步开通即时通信 IM 服务。 即时通信 IM 属于增值服务，详细计费规则请参见 [即时通信 IM 价格说明](https://cloud.tencent.com/document/product/269/11673)。

### 第二步：构建并运行应用程序
1. 下载源码：
   git clone https://github.com/tencentyun/TUILiveKit.git
2. 使用 Android Studio（3.5及以上的版本）打开源码工程`TUILiveKit` 并找到并打开`TUILiveKit/Android/debug/src/main/java/com/tencent/liteav/debug/GenerateTestUserSig.java`文件。
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

