English | [简体中文](README.zh.md)

# TUILiveKit 

This document describes how to quickly complete the TUILiveKit example project and experience high-quality live. With this document, you can run through the Demo in 10 minutes and start your own live.

<img src="https://web.sdk.qcloud.com/trtc/live/web/image/pc-layout-en.png"/>

<img src="https://web.sdk.qcloud.com/trtc/live/web/image/h5-layout-en.png"/>

## Prerequisites

- Node.js version: Node.js ≥ 16.19.1 (we recommend using the official LTS version, please match the npm version with the node version).
- Modern browser，supporting [WebRTC APIs](https://caniuse.com/?search=webrtc)。

## Download Demo

1. Open the Terminal, copy and paste the sample command to clone the repository.
   ```bash
   git clone https://github.com/Tencent-RTC/TUILiveKit.git
   ```
2. Install dependencies.
   ```bash
   cd TUILiveKit/Web/web-vite-vue3

   npm install
   ```

## Configure Demo
1. [Activate the TUILiveKit service](https://trtc.io/document/60033?platform=ios&product=live)，get the **SDKAppID** and **SDKSecretKey**.
   <img src="https://web.sdk.qcloud.com/trtc/live/web/image/active-livekit-en.png"/>

2. Open the `TUILiveKit/Web/web-vite-vue3/src/config/basic-info-config.js` file and enter the **SDKAppID** and **SDKSecretKey** you got when you activated the service:
   <img src="https://web.sdk.qcloud.com/trtc/live/web/image/supplementary-config.png"/>
## Run Demo
Run Demo by typing the command in the terminal.
   ```bash
   #cd TUILiveKit/Web/web-vite-vue3
   npm run dev
   ```

`npm run dev` does **not** start `upload-server` by default.
If you want one-command startup with upload-server, run `npm run dev:with-upload-server`.

### Optional: integrate upload-server for cover upload

1. Prepare environment file

```bash
cp upload-server/.env.example upload-server/.env
```

2. Configure one provider in `upload-server/.env`
- `STORAGE_PROVIDER=cos`: set `COS_SECRET_ID`, `COS_SECRET_KEY`, `COS_BUCKET`, `COS_REGION`
- `STORAGE_PROVIDER=custom`: set `CUSTOM_UPLOAD_URL` and related fields

3. Install and run upload-server

```bash
npm run upload-server:bootstrap
npm run upload-server:standalone
```

`upload-server` is an independent Node project. On a fresh environment, you must run
`npm run upload-server:bootstrap` at least once before starting it.

4. Verify service
- `http://127.0.0.1:3071/api/test`
- `http://127.0.0.1:3071/api/upload/config`

If the upload-server is unavailable or provider is not configured, the UI falls back to manual cover URL input.

5. Configure renderer upload API base URL (`VUE_APP_UPLOAD_SERVER_BASE_URL`)

By default, the Web demo requests `http://127.0.0.1:3071`.
If your upload service is deployed remotely, set `VUE_APP_UPLOAD_SERVER_BASE_URL`:
`VUE_APP_UPLOAD_SERVER_BASE_URL` should be protocol + domain only (without trailing slash or path),
for example: `https://upload.example.com`.

- One-time for local start:

```bash
VUE_APP_UPLOAD_SERVER_BASE_URL=https://your-upload-domain npm run dev
```

- One-time for build:

```bash
VUE_APP_UPLOAD_SERVER_BASE_URL=https://your-upload-domain npm run build
```

- Persistent mode-based config (recommended):
  - Add `VUE_APP_UPLOAD_SERVER_BASE_URL=...` in your local `.env` / mode env file
  - Use the corresponding mode when running `dev` or `build`

## Pack Demo

1. Execute the following command to pack the dist file.
   ```bash
   npm run build
   ```
   If the build appears to pause after `modules transformed`, see [docs/VITE-BUILD.md](docs/VITE-BUILD.md). For a faster **modern-browsers-only** bundle (skips IE 11 legacy pass): `npm run build:modern`.
2. Deploy the dist file to your server.

## Host login (go live)

Step-by-step host login and **Start live**: [docs/HOST-LOGIN.md](docs/HOST-LOGIN.md).

## About more
- [Client APIs](https://trtc.io/document/64181?platform=ios&product=live)
- [Product Introduction](https://trtc.io/document/60034?platform=electron&product=live)
- [Activate the Service](https://trtc.io/document/60033?platform=ios&product=live)
