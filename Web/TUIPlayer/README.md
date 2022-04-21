# TUIPlayer 快速跑通

简体中文 | [English](./README.en.md)

本文档主要介绍如何跑通 Web TUIPlayer 组件，快速接入 Web 推流场景，更详细的 TUIPlayer 组件介绍，请点击腾讯云官网文档： [**Web 直播互动组件** ](https://cloud.tencent.com/document/product/647/63830)...

## 目录结构

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
│   │   ├── common   // 基础组件
│   │   ├── comp-header  // 顶部栏组件
│   │   ├── comp-message  // 聊天消息组件
│   │   ├── comp-player   // 播放组件
│   │   └── mixin
│   ├── config
│   │   └── basic-info-config.js     // TUIPlayer 信息配置文件
│   ├── constants
│   │   ├── mutation-types.js
│   │   └── room.js
│   ├── locales        // 国际化
│   │   ├── i18n.js
│   │   └── lang
│   ├── main.js        // 入口文件
│   ├── player.vue     // 拉流页面
│   ├── store          // vuex全局存储
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
└── vue.config.js
```

## 运行示例

1. 安装依赖

   ```bash
   npm install
   ```

2. 配置文件

   在 [实时音视频控制台](https://console.cloud.tencent.com/trtc)获取 SDKAPPID 和 SECRETKEY 并填写到 `TUIPlayer/src/config/basic-info-config.js` 中；

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

## 开发说明

技术栈：Vue2 + pug + stylus

UI组件库：element-ui

1. TUIPlayer 使用按需引入的方式引用 element-ui 组件，如需添加 element-ui 组件，请在 `src/main.js` 文件中确认组件是否被引入。

2. TUIPlayer 使用 `#006EFF` 作为element-ui主题色，更改element-ui主题色请参考 [element-ui 在线主题生成]([https://element.eleme.cn/#/zh-CN/component/custom-theme)

3. 引入图标的用法 

   1）TUIPlayer 在 `src/assets/icons/svg` 路径下统一管理 svg 图标

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
