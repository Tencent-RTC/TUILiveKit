# TUIPusher & TUIPlayer 快速跑通

简体中文 | [English](./README.en.md)

本文档主要介绍如何跑通 Web TUIPusher & TUIPlayer，快速接入 Web 推拉流场景，更详细的 TUIPusher & TUIPlayer 组件介绍，请点击腾讯云官网文档： [**Web 直播互动组件** ](https://cloud.tencent.com/document/product/647/63830)...

## 接入方式

### 注意事项

- TUIPusher & TUIPlayer 基于腾讯云实时音视频和即时通信服务进行开发。实时音视频 TRTC 应用与 即时通信 IM 应用的 SDKAppID 一致，才能复用账号与鉴权。
- 即时通信 IM 应用针对文本消息，提供基础版本的 [安全打击](https://cloud.tencent.com/document/product/269/47170) 能力，如果希望使用自定义不雅词功能，可以单击 **升级** 或在 [购买页](https://buy.cloud.tencent.com/avc?position=1400399435) 购买 **安全打击-高级版** 服务。
- 本地计算 UserSig 的方式仅用于本地开发调试，请勿直接发布到线上，一旦 SECRETKEY 泄露，攻击者就可以盗用您的腾讯云流量。正确的 UserSig 签发方式是将 UserSig 的计算代码集成到您的服务端，并提供面向 App 的接口，在需要 UserSig 时由您的 App 向业务服务器发起请求获取动态 UserSig。更多详情请参见 [服务端生成 UserSig](https://cloud.tencent.com/document/product/269/32688#GeneratingdynamicUserSig)。

### 步骤一：开通腾讯云服务

#### 方式1：基于实时音视频

步骤1：创建实时音视频 TRTC 应用

1. [注册腾讯云账号](https://cloud.tencent.com/register?s_url=https%3A%2F%2Fcloud.tencent.com%2Fdocument%2Fproduct%2F647%2F49327) 并开通 [实时音视频](https://console.cloud.tencent.com/trtc) 和 [即时通信](https://console.cloud.tencent.com/im) 服务。 
2. 在 [实时音视频控制台](https://console.cloud.tencent.com/trtc) 单击 **应用管理 > 创建应用** 创建新应用。 ![创建应用](https://main.qcloudimg.com/raw/34f87b8c0a817d8d3e49baac5b82a1fa.png)

步骤2: 获取 TRTC 密钥信息

1. 在 **应用管理 > 应用信息** 中获取 SDKAppID 信息。    
![img](https://qcloudimg.tencent-cloud.cn/raw/f7915fbbeb48518c2b25a413960f3432.png)
2. 在 **应用管理 > 快速上手** 中获取应用的 secretKey 信息。     
![img](https://qcloudimg.tencent-cloud.cn/raw/06d38bbdbaf43e1f2b444edae00019fa.png)

> ?
>
> - 首次创建实时音视频应用的腾讯云账号，可获赠一个10000分钟的音视频资源免费试用包。
> - 创建实时音视频应用之后会自动创建一个 SDKAppID 相同的即时通信 IM 应用，可在 [即时通信控制台](https://console.cloud.tencent.com/im) 配置该应用的套餐信息。 

#### 方式2：基于即时通信

步骤1：创建即时通信 IM 应用

1. 登录 [即时通信 IM 控制台](https://console.cloud.tencent.com/im)，单击 **创建新应用** 将弹出对话框。   
![img](https://main.qcloudimg.com/raw/c8d1dc415801404e30e49ddd4e0c0c13.png)
2. 输入您的应用名称，单击 **确认** 即可完成创建。      
![img](https://main.qcloudimg.com/raw/496cdc614f7a9d904cb462bd4d1e7120.png)
3. 您可在 [即时通信 IM 控制台](https://console.cloud.tencent.com/im) 总览页面查看新建应用的状态、业务版本、SDKAppID、创建时间以及到期时间。请记录 SDKAppID 信息。

步骤2：获取 IM 密钥并开通实时音视频服务

1. 在 [即时通信 IM 控制台](https://console.cloud.tencent.com/im) 总览页单击您创建完成的即时通信 IM 应用，随即跳转至该应用的基础配置页。在 **基本信息** 区域，单击 **显示密钥**，复制并保存密钥信息。    
![img](https://main.qcloudimg.com/raw/030440f94a14cd031476ce815ed8e2bc.png)

> !请妥善保管密钥信息，谨防泄露。

2. 在该应用的基础配置页，开通腾讯云实时音视频服务。    
![img](https://main.qcloudimg.com/raw/1c2ce5008dad434d9206aabf0c07fd04.png)



### 步骤二：项目准备

1. 下载 TUIPusher & TUIPlayer 代码。
2. 为 TUIPusher & TUIPlayer 安装依赖。

```bash
cd Web/TUIPusher
npm install

cd Web/TUIPlayer
npm install
```

3. 将 sdkAppId 和 secretKey 填入 `TUIPusher/src/config/basic-info-config.js` 及 `TUIPlayer/src/config/basic-info-config.js` 配置文件中。 ![img](https://qcloudimg.tencent-cloud.cn/raw/9286fcb781fa37179f84e4bdcd85bfae.png)

4. 本地开发环境运行 TUIPusher & TUIPlayer。

```bash
cd Web/TUIPusher
npm run serve

cd Web/TUIPlayer
npm run serve
```

5. 可打开 `http://localhost:8080` 和 `http://localhost:8081` 体验 TUIPusher 和 TUIPlayer 功能。

6. 可更改 `TUIPusher/src/config/basic-info-config.js` 及 `TUIPlayer/src/config/basic-info-config.js` 配置文件中的房间，主播及观众等信息，**注意保持 TUIPusher 和 TUIPlayer 的房间信息，主播信息一致**。

> !
>
> - 完成以上配置，您可以使用 TUIPusher & TUIPlayer 进行超低延时直播，如您需要支持快直播和标准直播，请继续阅读 [步骤三：旁路直播](https://file+.vscode-resource.vscode-webview.net/Users/lixin/Documents/tencent/TUILiveRoom/Web/README.md#step3)。
> - 本地计算 UserSig 的方式仅用于本地开发调试，请勿直接发布到线上，一旦您的 `SECRETKEY` 泄露，攻击者就可以盗用您的腾讯云流量。
> - 正确的 UserSig 签发方式是将 UserSig 的计算代码集成到您的服务端，并提供面向 App 的接口，在需要 UserSig 时由您的 App 向业务服务器发起请求获取动态 UserSig。更多详情请参见 [服务端生成 UserSig](https://cloud.tencent.com/document/product/269/32688#GeneratingdynamicUserSig)。

### 步骤三：旁路直播

TUIPusher & TUIPlayer 实现的快直播和标准直播依托于腾讯云 [云直播服务](https://cloud.tencent.com/document/product/267)，因此支持快直播和标准直播线路需要您开启旁路推流功能。

1. 在 [**实时音视频控制台**](https://console.cloud.tencent.com/trtc) 中为您正在使用的应用开启旁路推流配置，可按需开启指定流旁路或全局自动旁路。 ![img](https://main.qcloudimg.com/raw/b9846f4a7f5ce1e39b3450963e872c90.png)
2. 请在 [**域名管理**](https://console.cloud.tencent.com/live/domainmanage) 页面添加自有播放域名，具体请参见 [添加自有域名](https://cloud.tencent.com/document/product/267/20381)。
3. 在 `TUIPlayer/src/config/basic-info-config.js` 配置文件中配置播放域名。

完成以上配置，您可以体验 TUIPusher & TUIPlayer 支持超低延时直播，快直播以及标准直播的所有功能。

## 其他

在后续的迭代中, TRTC Web 端推拉流组件会逐渐与 iOS、Android 等各端连通，并在 Web 端实现观众连麦、高级美颜、自定义布局、转推多平台、上传图片文字音乐等能力，欢迎大家多多使用、提出您的宝贵意见。

如果有任何需要或者反馈，可扫描下方二维码，或者单击 [反馈链接](https://cloud.tencent.com/apply/p/jpkje0im7a) 同步给我们。  
<img width="200px" height="200px" src="https://qcloudimg.tencent-cloud.cn/raw/d2e33e2d5bc6c584ddd5eb7830e92311.png"></img>

此外，我们欢迎加入 TUI 组件使用交流 QQ 群（群号：592465424）进行技术交流和问题反馈。  
![img](https://main.qcloudimg.com/raw/1ea3ab1ff36d37c889f4140499585a4a.png)