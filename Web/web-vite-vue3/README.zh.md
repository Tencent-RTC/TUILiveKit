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

`npm run dev` 默认**不包含** `upload-server`。
如果希望一条命令同时启动 upload-server，可执行 `npm run dev:with-upload-server`。

### 第二步补充（可选）：对接 upload-server 实现封面上传

1. 准备环境文件

```bash
cp upload-server/.env.example upload-server/.env
```

2. 配置 `upload-server/.env`（二选一）
- `STORAGE_PROVIDER=cos`：配置 `COS_SECRET_ID`、`COS_SECRET_KEY`、`COS_BUCKET`、`COS_REGION`
- `STORAGE_PROVIDER=custom`：配置 `CUSTOM_UPLOAD_URL` 及相关字段

3. 安装并启动 upload-server

```bash
npm run upload-server:bootstrap
npm run upload-server:standalone
```

`upload-server` 是独立 Node 子项目。在全新环境（或 `upload-server/node_modules` 不存在）时，
必须至少先执行一次 `npm run upload-server:bootstrap`。

4. 验证服务可用性
- `http://127.0.0.1:3071/api/test`
- `http://127.0.0.1:3071/api/upload/config`

若 upload-server 不可用或 provider 未配置，界面会自动回退到手动输入封面 URL。

5. 配置渲染进程上传服务地址（`VUE_APP_UPLOAD_SERVER_BASE_URL`）

Web demo 默认请求 `http://127.0.0.1:3071`。
如果图片上传服务部署在远端，请设置 `VUE_APP_UPLOAD_SERVER_BASE_URL`：
`VUE_APP_UPLOAD_SERVER_BASE_URL` 建议配置为“协议 + 域名”（不带尾部斜杠、不带路径），
例如：`https://upload.example.com`。

- 本次启动临时生效：

```bash
VUE_APP_UPLOAD_SERVER_BASE_URL=https://your-upload-domain npm run dev
```

- 本次构建临时生效：

```bash
VUE_APP_UPLOAD_SERVER_BASE_URL=https://your-upload-domain npm run build
```

- 推荐方式（按模式持久化）：
  - 在本地 `.env` 或对应 mode 的 env 文件中增加 `VUE_APP_UPLOAD_SERVER_BASE_URL=...`
  - 启动或构建时使用对应 mode

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
