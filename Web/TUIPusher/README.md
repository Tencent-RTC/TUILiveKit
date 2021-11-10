## 前言

TUIPusher 是结合腾讯云实时音视频SDK(TRTC) 及腾讯云即时通讯SDK(IM) 开发的包含UI的直播端推流场景化解决方案。TUIPusher 会帮助您快速搭建自己的直播推流业务，助力业务扩张。为了您更加快速的体验 TUIPusher 的功能，我们结合用户管理系统和房间管理系统提供了 [TUIPusher Demo](https://web.sdk.qcloud.com/component/tuiliveroom/tuipusher/pusher.html)。

<img width="1280" src="../liveroom_pusher.gif"/>

## 功能介绍

+ 支持采集麦克风和扬声器的流并推流
  - 可根据需求设置视频参数（帧率，分辨率，码率）
  - 支持开启美颜并设置视频美颜参数

+ 支持采集屏幕分享流并推流

+ 支持推流到腾讯云实时音视频后台，推流到腾讯云 CDN

+ 支持在线聊天室，和在线观众进行聊天互动

+ 支持获取观众列表，对在线观众进行禁言操作

## 接入准备

请参考：[Web互动直播极速集成（含UI）](https://cloud.tencent.com/document/product/647/63830)

## 安装及运行

1. 安装依赖

   ```bash
   npm install
   ```

2. 配置文件

   在 [实时音视频控制台](https://console.cloud.tencent.com/trtc)获取 SDKAPPID 和 SECRETKEY 并填写到 `TUIPusher/src/config/basic-info-config.js` 中；

3. 开发环境运行

   ```bash
   npm run serve
   ```

4. 打包

   ```bash
   npm run build
   ```

5. eslint检查及自动修复

   ```bash
   npm run lint
   ```

## 目录说明

```
.
├── README.md
├── babel.config.js
├── package-lock.json
├── package.json
├── public
│   ├── favicon.ico
│   └── index.html
├── src
│   ├── assets
│   │   ├── icons   // 项目中用到的 svg icons
│   │   ├── img     // 项目中使用到的图片资源
│   │   └── style   // 包括 element-ui 自定义主题样式文件以及全局样式文件
│   ├── components
│   │   ├── common         // 基础组件
│   │   ├── comp-chat      // 聊天组件
│   │   ├── comp-header    // 顶部栏组件
│   │   ├── comp-live-setting       // 直播设置组件
│   │   ├── comp-live-stream        // 直播推流组件
│   │   ├── comp-participants       // 观众列表组件
│   │   ├── comp-pre-setting        // 直播预设置组件
│   │   ├── comp-screen-share       // 屏幕分享组件
│   │   └── mixin                   // 混入（推流功能，屏幕分享功能，聊天功能）
│   ├── config
│   │   └── basic-info-config.js    // 基础信息配置文件（包括 sdkAppId, secretKey, userId等信息）
│   ├── constants
│   │   ├── mutation-types.js       // mutation 提交
│   │   └── room.js                 // 常量
│   ├── locales           // 国际化
│   │   ├── i18n.js
│   │   └── lang
│   ├── main.js     // 入口文件
│   ├── pusher.vue  // 推流页面
│   ├── store             // vuex全局存储
│   │   ├── actions.js
│   │   ├── getters.js
│   │   ├── index.js
│   │   ├── mutations.js
│   │   └── state.js
│   └── utils
│       ├── _eventBus.js  // eventBus全局事件通讯
│       ├── common.js
│       ├── decodeText.js   // 聊天消息解析
│       ├── emojiMap.js     // 聊天消息表情解析
│       ├── lib-generate-test-usersig.min.js  // 本地生成userSig的文件
│       └── utils.js
└── vue.config.js            // vue配置文件
```

## 开发说明

技术栈：Vue2 + pug + stylus

UI组件库：element-ui

1. TUIPusher 使用按需引入的方式引用 element-ui 组件，如需添加 element-ui 组件，请在 `src/main.js` 文件中确认组件是否被引入。

2. TUIPusher使用 `#006EFF` 作为element-ui主题色，更改element-ui主题色请参考 [element-ui 在线主题生成]([https://element.eleme.cn/#/zh-CN/component/custom-theme)

3. 引入图标的用法 

   1）TUIPusher 在 `src/assets/icons/svg` 路径下统一管理 svg 图标

   2）使用全局组件 svg-icon 加载 svg 图标，注意 icon-name 绑定的值与 src/assets/icons/svg 路径下图标的文件名一致

   ```pug
   svg-icon(icon-name="info")
   ```

   3）支持添加样式修改svg的颜色和大小

   ```pug
   svg-icon.icon-style(icon-name="info")
   ```

   ```stylus
   .icon-style
     fill #000000
     width 16px
     height 16px
   ```
