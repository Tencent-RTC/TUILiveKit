本文档主要介绍如何快速集成实时音视频（TRTC）SDK，运行 TRTC 场景化解决方案，实现视频互动直播。

## 目录结构

```
TUILiveRoom
├─ App          // 主面板，场景入口
├─ Beauty       // 美颜面板，包含美颜，滤镜，动效等效果
├─ Debug        // 调试相关
└─ Source       // 互动直播业务逻辑
```

## 环境准备
- 最低兼容 Android 4.1（SDK API Level 16），建议使用 Android 5.0 （SDK API Level 21）及以上版本
- Android Studio 3.5及以上版本
- App 要求 Android 4.1及以上设备

## 运行示例

## 第一步：创建TRTC的应用
1. 一键进入腾讯云实时音视频控制台的[应用管理](https://console.cloud.tencent.com/trtc/app)界面，选择创建应用，输入应用名称，例如 `TUIKitDemo` ，单击 **创建**；
2. 点击对应应用条目后**应用信息**，具体位置如下下图所示：
    <img src="https://qcloudimg.tencent-cloud.cn/raw/62f58d310dde3de2d765e9a460b8676a.png" width="900">
3. 进入应用信息后，按下图操作，记录SDKAppID和密钥：
    <img src="https://qcloudimg.tencent-cloud.cn/raw/bea06852e22a33c77cb41d287cac25db.png" width="900">
4. 进入腾讯云直播[LICENSE管理](https://console.cloud.tencent.com/live/license)节面，创建应用并绑定LICENSE
![](https://qcloudimg.tencent-cloud.cn/raw/886dbc5cf9cea301a69a7c06c80390d4.png)
创建成功后请记录 ` License Key `和 `License URL`，便于在运行时使用。
![](https://qcloudimg.tencent-cloud.cn/raw/5bca99c4b00f23eaa763310dc475ec1e.png)
5. [配置推拉流域名](https://console.cloud.tencent.com/live/domainmanage)

### 第三步：下载源码，配置工程
1. 使用 Android Studio（3.5及以上的版本）打开源码工程`TUILiveRoom `。
2. 找到并打开`TUILiveRoom/Debug/src/main/java/com/tencent/liteav/debug/GenerateTestUserSig.java`文件。
3. 设置`GenerateTestUserSig.java`文件中的相关参数：
<ul style="margin:0"><li/>SDKAPPID：默认为占位符（PLACEHOLDER），请设置为实际的 SDKAppID。
<li/>SECRETKEY：默认为占位符（PLACEHOLDER），请设置为实际的密钥信息。</ul>
<img src="https://liteav.sdk.qcloud.com/doc/res/trtc/picture/zh-cn/sdkappid_secretkey.png">

4. 返回实时音视频控制台，单击【粘贴完成，下一步】。
5. 单击【关闭指引，进入控制台管理应用】。

>!本文提到的生成 UserSig 的方案是在客户端代码中配置 SECRETKEY，该方法中 SECRETKEY 很容易被反编译逆向破解，一旦您的密钥泄露，攻击者就可以盗用您的腾讯云流量，因此**该方法仅适合本地跑通工程和功能调试**。
>正确的 UserSig 签发方式是将 UserSig 的计算代码集成到您的服务端，并提供面向 App 的接口，在需要 UserSig 时由您的 App 向业务服务器发起请求获取动态 UserSig。更多详情请参见 [服务端生成 UserSig](https://cloud.tencent.com/document/product/647/17275#Server)。

### 第四步：编译运行
使用 Android Studio（3.5及以上的版本）打开源码工程`TUILiveRoom/Android `，等待Android Studio工程同步完成后，连接真机点击运行按钮即可体验本APP



### 第五步：体验应用（**体验应用至少需要两台设备**）

#### 设备 A

步骤1、输入用户名(<font color=red>请确保用户名唯一性，不能与其他用户重复</font>)，比如111：
步骤2、点击创建房间
步骤3、输入房间名称，点击开始直播；

#### 设备 B

步骤1、输入用户名(<font color=red>请确保用户名唯一性，不能与其他用户重复</font>)，比如222：

步骤2、输入设备 A 创建的房间号，点击进入房间（<font color=red>请注意，房间号设备 A 的房间顶部查看</font>）

## 常见问题

- [TUI 场景化解决方案常见问题](https://cloud.tencent.com/developer/article/1952880)
- [联系我们](https://cloud.tencent.com/document/product/647/70641)
