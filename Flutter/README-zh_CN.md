# 快速跑通Flutter示例工程

_[English](README.md) | 简体中文_

<img src="https://qcloudimg.tencent-cloud.cn/raw/ec034fc6e4cf42cae579d32f5ab434a1.png" align="left" width=65 height=65>TUILiveKit 是腾讯云推出一款互动直播的含 UI 组件，通过集成该组件，您只需要编写几行代码就可以为您的 App 添加互动直播功能，并且支持互动连麦、聊天弹幕、心动点赞、互动礼物以及音效变声等功能。

## 环境准备

### Flutter
- Flutter 3.22.0 或更高版本。
- Dart 3.4.0 或更高的版本。

### Android
- Android Studio 3.5及以上版本。
- Android 5.0 及以上版本的 Android 设备。

### iOS
- Xcode 13.0及以上版本。
- 请确保您的项目已设置有效的开发者签名。


## 跑通示例

按照以下步骤，运行示例工程。

### 开通服务

1. TUILiveKit 目前暂不支持自助开通，如需使用，您可以 [提交申请](https://cloud.tencent.com/apply/p/aro4yu3iy18)，申请成功后即可获得14天的体验版，可免费体验 TUILiveKit 的全部功能。
2. 申请成功后，会获取到SDKAppID和SDKSecretKey

### 构建并运行示例

#### 1. 下载代码

```
git clone https://github.com/Tencent-RTC/TUILiveKit.git
```

#### 2. 通过 Android Studio 打开 TUILiveKit/Flutter 项目

#### 3. 配置 SDKAppID 和 SDKSecretKey

打开Flutter/example/lib/debug/generate_test_user_sig.dart文件，将  提交申请 时获取到的对应的 SDKAppID 和 SDKSecretKey 填入其中：

```
static int sdkAppId = 0;
static String secretKey = '';
```

#### 4. 编译、运行示例工程，并在iOS设备和Android设备上安装 APP

## 功能体验

1. 使用两台手机（A、B）用字符串作为自己的用户ID登录应用,如果第一次登录需要添加用户名
2. 手机A上的用户点击开始直播按钮发起直播
3. 手机B上可以下拉刷新房间列表，点击即可进入直播间观看直播
