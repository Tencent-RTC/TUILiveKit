# 快速跑通iOS示例工程

_[English](README.md) | 简体中文_

<img src="https://qcloudimg.tencent-cloud.cn/raw/ec034fc6e4cf42cae579d32f5ab434a1.png" align="left" width=65 height=65>TUILiveKit 是腾讯云推出一款互动直播的含 UI 组件，通过集成该组件，您只需要编写几行代码就可以为您的 App 添加互动直播功能，并且支持互动连麦、聊天弹幕、心动点赞、互动礼物以及音效变声等功能。

## 环境准备

- Xcode 13 及以上
- iOS 13.0 及以上

## 跑通示例

按照以下步骤，运行示例工程。

### 开通服务

1. 一键进入腾讯云实时音视频控制台的[应用管理](https://console.trtc.io/app)界面，选择创建应用，输入应用名称，例如 `TUIKitDemo`，单击 **创建**；
2. 点击对应应用条目后**应用信息**，记录 SDKAppID 和密钥：
   - SDKAppID：`TUIKitDemo`后括号里的一串数字。
   - SDKSecretKey：点击**复制SDKSecretKey**即可。

### 构建并运行示例

#### 1. 下载代码

```
git clone https://github.com/Tencent-RTC/TUILiveKit.git
```

#### 2. 加载依赖库

```
cd TUILiveKit/iOS/Example
pod install
```

#### 3. 配置 SDKAppID 和 SDKSecretKey

你需要在 `GenerateTestUserSig.swift`文件中，配置应用的`SDKAppID` 和 `SDKSecretKey`。

```
let SDKAPPID: Int = 0
let SECRETKEY = ""
```

#### 4. 编译、运行示例工程，并在两台 iOS 设备上安装 APP

## 功能体验

1. 使用两台手机（A、B）用字符串作为自己的用户ID登录应用,如果第一次登录需要添加用户名
2. 手机A上的用户点击开始直播按钮发起直播（注意选择视频直播或者语音直播）
3. 手机B上的用户输入手机A上用户的用户ID，点击加入直播或者加入语聊房观看直播

## 常见问题
#### “Sandbox: rsync”，错误详情：

```
Sandbox: rsync.samba(2564) deny(1) file-write-unlink /Users/wesleylei/Library/Developer/Xcode/DerivedData/TestLiveKit-etglzzsjcwgokmcvmmnjifiqfgfx/Build/Products/Debug-iphoneos/TestLiveKit.app/Frameworks/Kingfisher.framework/_CodeSignature

```

#### “SDK does not contain”，错误详情：
```
clang: error: SDK does not contain 'libarclite' at the path '/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/arc/libarclite_iphoneos.a'; try increasing the minimum deployment target
```
#### “Linker command failed with exit code 1 (use -v to see invocation)”，错误详情：

```
ld: Undefined symbols:
  _OBJC_CLASS_$_SDImageCoderHelper, referenced from:
       in TUITool.o
clang: error: linker command failed with exit code 1 (use -v to see invocation)
```
请参考：[常见问题](https://trtc.io/zh/document/60048?platform=ios&product=live&menulabel=uikit)

