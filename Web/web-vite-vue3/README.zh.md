# TUILiveKit 

本文档主要介绍如何快速跑通 TUILiveKit 示例工程，体验高质量在线直播。跟随本文档，您可以在 10 分钟内跑通 Demo，并开启一场属于您自己的直播。

<img src="https://web.sdk.qcloud.com/trtc/live/web/image/pc-layout.png"/>

<img src="https://web.sdk.qcloud.com/trtc/live/web/image/h5-layout.png"/>

## 环境准备

- Node.js 版本：Node.js ≥ 16.19.1（推荐使用官方 LTS 版本，npm 版本请与 node 版本匹配）。
- 现代浏览器，支持 [WebRTC APIs](https://cloud.tencent.com/document/product/647/17249)。

## 下载 Demo

1. 打开终端复制输入示例命令克隆仓库。
   ```bash
   git clone https://github.com/Tencent-RTC/TUILiveKit.git
   ```
2. 安装依赖
   ```bash
   cd TUILiveKit/Web/web-vite-vue3

   npm install
   ```

## 配置 Demo
1. 激活 [TUILiveKit](https://cloud.tencent.com/document/product/647/105439) 服务。SDKAppID 和 SDKSecretKey。
   <img src="https://web.sdk.qcloud.com/trtc/live/web/image/active-livekit.png"/>

2. 打开 `TUILiveKit/Web/web-vite-vue3/src/config/basic-info-config.js` 文件，并输入激活服务时获得的 `SDKAppID` 和 `SDKSecretKey`:
   <img src="https://web.sdk.qcloud.com/trtc/live/web/image/supplementary-config.png"/>
## 跑通 Demo

在终端中输入命令，运行 Demo。
   ```bash
   #cd TUILiveKit/Web/web-vite-vue3
   npm run dev
   ```

## 打包部署

1. 执行如下命令打包 dist 文件。
   ```bash
   npm run build
   ```
2. 将 dist 文件部署到您的服务器上。

## 关于更多
- [客户端 API 文档](https://cloud.tencent.com/document/product/647/81970)
- [Livekit 组件介绍](https://cloud.tencent.com/document/product/647/105438)
- [Livekit 开通服务](https://cloud.tencent.com/document/product/647/105439)