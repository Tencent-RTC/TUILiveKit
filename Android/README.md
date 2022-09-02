# TUILiveRoom Android 示例工程快速跑通

_中文 | [English](README.en.md)_

本文档主要介绍如何快速跑通TUILiveRoom示例工程，体验高质量语音互动，更详细的TUILiveRoom组件接入流程，请点击腾讯云官网文档： [**TUILiveRoom组件 Android 接入说明** ](https://cloud.tencent.com/document/product/647/43182)

## 目录结构

```
TUILiveRoom
├─ app             // 主面板，场景入口
├─ debug           // 调试相关
├─ tuibeauty       // 美颜面板，包含美颜，滤镜，动效等效果
├─ tuigift         // 点赞功能核心组件
├─ tuiaudioeffect  // 音效管理功能核心组件
├─ tuibarrage      // 弹幕功能核心组件
└─ tuiliveroom     // 互动直播业务逻辑
```

## 环境准备

- 最低兼容 Android 4.1（SDK API Level 16），建议使用 Android 5.0 （SDK API Level 21）及以上版本
- Android Studio 3.5及以上版本
- App 要求 Android 4.1及以上设备

## 运行示例

## 第一步：创建TRTC的应用

1. 一键进入腾讯云实时音视频控制台的[应用管理](https://console.cloud.tencent.com/trtc/app)界面，选择创建应用，输入应用名称，例如 `TUIKitDemo`，单击 **创建**；
2. 点击对应应用条目后**应用信息**，具体位置如下下图所示：
   <img src="https://qcloudimg.tencent-cloud.cn/raw/62f58d310dde3de2d765e9a460b8676a.png" width="900">
3. 进入应用信息后，按下图操作，记录SDKAppID和密钥：
   <img src="https://qcloudimg.tencent-cloud.cn/raw/bea06852e22a33c77cb41d287cac25db.png" width="900">

> ! 本功能同时使用了腾讯云 [实时音视频 TRTC](https://cloud.tencent.com/document/product/647/16788) 和 [即时通信 IM](https://cloud.tencent.com/document/product/269) 两个基础 PaaS 服务，开通实时音视频后会同步开通即时通信 IM 服务。 即时通信 IM 属于增值服务，详细计费规则请参见 [即时通信 IM 价格说明](https://cloud.tencent.com/document/product/269/11673)。

### 第二步：下载源码，配置工程

1. 使用 Android Studio（3.5及以上的版本）打开源码工程`TUILiveRoom `。
2. 找到并打开`TUILiveRoom/Android/debug/src/main/java/com/tencent/liteav/debug/GenerateTestUserSig.java`文件。
3. 设置`GenerateTestUserSig.java`文件中的相关参数：

<ul>
<li>SDKAPPID：默认为 0 ，请设置为实际申请的SDKAPPID。</li>
<li>SECRETKEY：默认为空字符串，请设置为实际申请的SECRETKEY。</li>
<li>XMAGIC_LICENSE_URL【可选】：默认为空字符串，美颜特效License申请请前往: <a href="https://cloud.tencent.com/document/product/616/65878">腾讯特效 License</a>。</li>
<li>XMAGIC_LICENSE_KEY【可选】：默认为空字符串，美颜特效License申请请前往: <a href="https://cloud.tencent.com/document/product/616/65878">腾讯特效 License</a>。</li>
</ul>
<img src="https://liteav.sdk.qcloud.com/doc/res/trtc/picture/zh-cn/sdkappid_secretkey.png">

### 第三步：编译运行

使用 Android Studio（3.5及以上的版本）打开源码工程`TUILiveRoom/Android `，等待Android Studio工程同步完成后，连接真机点击运行按钮即可体验本APP

### 第四步：体验应用（**体验应用至少需要两台设备**）

**设备 A（userId：111）**

- 步骤1：在欢迎页，输入用户名(请确保用户名唯一性，不能与其他用户重复)，比如111；
- 步骤2、点击创建房间；
- 步骤3、输入房间主题，点击开始直播；
- 步骤4、创建成功后，就进到了视频直播的主界面，此时记录一下房间号；

| 步骤1 | 步骤2 | 步骤3 | 步骤4|
|---------|---------|---------|---------|
| <img src="https://qcloudimg.tencent-cloud.cn/raw/24a76a18049eda3bdb6414493d43e286.png" width="250"> | <img src="https://qcloudimg.tencent-cloud.cn/raw/8f9290c8dfc3eaa44f3c0a82e776c497.png" width="250"> | <img src="https://qcloudimg.tencent-cloud.cn/raw/4ac45382e20f72a87b72104404eee2da.png" width="250"> |<img src="https://qcloudimg.tencent-cloud.cn/raw/01c73737f0af40fe17c70e1107a9f720.jpeg" width="250"> |

**设备 B（userId：222）**

- 步骤1：输入用户名(请确保用户名唯一性，不能与其他用户重复)，比如222；
- 步骤2、输入用户 A 创建的房间号（设备A第4步记录的房间号），点击加入房间；

| 步骤1 | 步骤2 |
|---------|---------|
| <img src="https://liteav.sdk.qcloud.com/doc/res/trtc/picture/zh-cn/user_b_ios.png" width="320"/> | <img src="https://qcloudimg.tencent-cloud.cn/raw/fe39e76723f304de52b9d677a8cebf97.png" width="320"/> |

## 常见问题

- [TUI 场景化解决方案常见问题](https://cloud.tencent.com/developer/article/1952880)
- [联系我们](https://cloud.tencent.com/document/product/647/70641)
