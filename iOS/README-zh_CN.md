# 快速跑通iOS示例工程

_[English](README.md) | 简体中文_

<img src="https://qcloudimg.tencent-cloud.cn/raw/ec034fc6e4cf42cae579d32f5ab434a1.png" align="left" width=65 height=65>TUILiveKit 是腾讯云推出一款互动直播的含 UI 组件，通过集成该组件，您只需要编写几行代码就可以为您的 App 添加互动直播功能，并且支持互动连麦、聊天弹幕、心动点赞、互动礼物以及音效变声等功能。

## 环境准备

- Xcode 13 及以上
- iOS 13.0 及以上

## 跑通示例工程

按照以下步骤，运行示例工程。

### 创建应用

1. 一键进入腾讯云实时音视频控制台的[应用管理](https://console.trtc.io/app)界面，选择创建应用，输入应用名称，例如 `TUIKitDemo`，单击 **创建**；
2. 点击对应应用条目后**应用信息**，记录 SDKAppID 和密钥：
   - SDKAppID：`TUIKitDemo`后括号里的一串数字。
   - SDKSecretKey：点击**复制SDKSecretKey**即可。

### 构建并运行示例工程

#### 1. 下载代码

```
$ git clone https://github.com/Tencent-RTC/TUILiveKit.git
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

## 编译报错？
1.出现“Sandbox: rsync”，编译报错截图：
<p align="center">
  <img src="https://write-document-release-1258344699.cos.ap-guangzhou.tencentcos.cn/100027182214/9d15ab110f6011ef8f3c525400f2c344.png"/>
</p>
可以在“Build Settings”中把"User Script Sandboxing"设置为“NO”
<p align="center">
  <img src="https://write-document-release-1258344699.cos.ap-guangzhou.tencentcos.cn/100027182214/9f56b9d80f4811efbab15254000ded98.png"/>
</p>

2.如果出现 SDK does not contain，编译报错截图：
<p align="center">
  <img src="https://write-document-release-1258344699.cos.ap-guangzhou.tencentcos.cn/100027182214/ab0bc21d0f4811ef98aa525400493f3c.png"/>
</p>
请在Podfile添加如下代码：

```
post_install do |installer|
  installer.pods_project.targets.each do |target|
    target.build_configurations.each do |config|
      config.build_settings['IPHONEOS_DEPLOYMENT_TARGET'] = '13.0'
    end
  end
end
```

3.如果在M系列电脑上运行模拟器，可能会出现“Linker command failed with exit code 1 (use -v to see invocation)”，编译报错截图：
<p align="center">
  <img src="https://write-document-release-1258344699.cos.ap-guangzhou.tencentcos.cn/100027182214/8a8f1cf50f5f11efbab15254000ded98.png"/>
</p>
需要修改xcode配置。xcode打开项目->Product-> Destination-> Destination Architectures 可以选择用哪种模式的模拟器打开，需要选择 （Rosetta）结尾的模拟器。
<p align="center">
  <img src="https://write-document-release-1258344699.cos.ap-guangzhou.tencentcos.cn/100027182214/6066c2f00f6011efbab15254000ded98.png"/>
</p>

更多问题[请点击查看](https://www.tencentcloud.com/zh/document/product/647/60048?lang=zh&pg=)


