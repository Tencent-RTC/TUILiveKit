[English](README.md) | 简体中文

# ultra-live-electron

本工程是一个桌面端直播推流工具，基于[腾讯云实时音视频技术](https://cloud.tencent.com/document/product/647)，快速开启在线直播。

## 前置依赖

- 操作系统要求: Windows 10+。
- 设备要求：需要有摄像头、扬声器、麦克风设备。
- 直播功能需要您开通[腾讯云实时音视频服务](https://cloud.tencent.com/document/product/647/44360)。

## 克隆代码并以开发模式运行

```
git clone https://github.com/Tencent-RTC/ultra-live-electron.git

cd ultra-live-electron

npm install

npm run start
```

## 构建安装包
构建好的安装包在 `release` 目录下。
```
npm run pack:win64
```

## 更多资料
0. [直通官网](https://cloud.tencent.com/document/product/647)
1. [腾讯云实时音视频](https://cloud.tencent.com/document/product/647)
2. [在线体验](https://cloud.tencent.com/document/product/647/17021)
3. API 文档:  [中文](https://web.sdk.qcloud.com/trtc/electron/doc/zh-cn/trtc_electron_sdk/TRTCCloud.html)、[English](https://web.sdk.qcloud.com/trtc/electron/doc/en-us/trtc_electron_sdk/TRTCCloud.html)
