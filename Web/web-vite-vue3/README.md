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

## Pack Demo

1. Execute the following command to pack the dist file.
   ```bash
   npm run build
   ```
2. Deploy the dist file to your server.

## About more
- [Client APIs](https://trtc.io/document/64181?platform=ios&product=live)
- [Product Introduction](https://trtc.io/document/60034?platform=electron&product=live)
- [Activate the Service](https://trtc.io/document/60033?platform=ios&product=live)