# 快速跑通iOS示例工程

_[English](README.md) | 简体中文_

<img src="https://qcloudimg.tencent-cloud.cn/raw/ec034fc6e4cf42cae579d32f5ab434a1.png" align="left" width=120 height=120>TUILiveKit 是腾讯云推出一款互动直播的含 UI 组件，通过集成该组件，您只需要编写几行代码就可以为您的 App 添加互动直播功能，并且支持互动连麦、聊天弹幕、心动点赞、互动礼物以及音效变声等功能。

## 环境准备

- Xcode 13 及以上
- iOS 13.0 及以上

## 跑通示例工程

按照以下步骤，运行示例工程。

### 创建应用

1.一键进入腾讯云实时音视频控制台的[应用管理](https://console.cloud.tencent.com/trtc/app)界面，选择创建应用，输入应用名称，例如 `TUIKitDemo`，单击 **创建**；
2. 点击对应应用条目后**应用信息**，具体位置如下下图所示：
   <img src="https://qcloudimg.tencent-cloud.cn/raw/62f58d310dde3de2d765e9a460b8676a.png" width="900">
3. 进入应用信息后，按下图操作，记录SDKAppID和密钥：
   <img src="https://qcloudimg.tencent-cloud.cn/raw/bea06852e22a33c77cb41d287cac25db.png" width="900">

> ! 本功能同时使用了腾讯云 [实时音视频 TRTC](https://cloud.tencent.com/document/product/647/16788) 和 [即时通信 IM](https://cloud.tencent.com/document/product/269) 两个基础 PaaS 服务，开通实时音视频后会同步开通即时通信 IM 服务。 即时通信 IM 属于增值服务，详细计费规则请参见 [即时通信 IM 价格说明](https://cloud.tencent.com/document/product/269/11673)。

### 构建并运行示例工程

#### 1. 下载代码

```
$ git clone git@github.com:tencentyun/TUILiveKit.git
```

#### 2. 加载依赖库

```
$ cd TUILiveKit/iOS/Example
$ pod install
```

#### 3. 配置 SDKAppID 和 SDKSecretKey

你需要在 `GenerateTestUserSig.swift`文件中，配置应用的`SDKAppID` 和 `SDKSecretKey`。

```
let SDKAPPID: Int = 0
let SECRETKEY = ""
```

#### 4. 编译、运行示例工程，并在两台 iOS 设备上安装 APP

## 体验 App

1. 使用两台手机（A、B）用字符串作为自己的用户ID登录应用,如果第一次登录需要添加用户名
2. 手机A上的用户点击开始直播按钮发起直播（注意选择视频直播或者语音直播）
3. 手机B上的用户输入手机A上用户的用户ID，点击加入直播或者加入语聊房观看直播
