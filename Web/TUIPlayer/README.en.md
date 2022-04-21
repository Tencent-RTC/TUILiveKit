# Quick Run of TUIPlayer

English | [简体中文](./README.md)

This document describes how to run the web TUIPlayer component to quickly connect to web push scenarios. For more information on the TUIPlayer component, see **[Integrating TUIPusher and TUIPlayer (Web)](https://intl.cloud.tencent.com/document/product/647/43303)** ...

## Directory Structure

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
│   │   ├── icons   // .svg icons used in the project
│   │   ├── img     // Image resources used in the project
│   │   └── style   // Contains the element-ui custom theme style and global style file
│   ├── components
│   │   ├── common   // Basic component
│   │   ├── comp-header  // Topbar component
│   │   ├── comp-message  // Chat message component
│   │   ├── comp-player   // Player component
│   │   └── mixin
│   ├── config
│   │   └── basic-info-config.js     // TUIPlayer information configuration file
│   ├── constants
│   │   ├── mutation-types.js
│   │   └── room.js
│   ├── locales        // Internationalization files
│   │   ├── i18n.js
│   │   └── lang
│   ├── main.js        // Entry file
│   ├── player.vue     // Pull page
│   ├── store          // Vuex global storage
│   │   ├── actions.js
│   │   ├── getters.js
│   │   ├── index.js
│   │   ├── mutations.js
│   │   └── state.js
│   └── utils
│       ├── _eventBus.js  // `eventBus` global event communication
│       ├── common.js
│       ├── decodeText.js   // Chat message parsing
│       ├── emojiMap.js     // Chat message emoji parsing
│       ├── lib-generate-test-usersig.min.js  // File for locally generating `userSig`
│       └── utils.js
└── vue.config.js
```

## Demo Run Example

1. Install dependencies.

   ```bash
   npm install
   ```

2. Configure the file.

   Get `SDKAPPID` and `SECRETKEY` in the [TRTC console](https://console.cloud.tencent.com/trtc) and enter them in `TUIPlayer/src/config/basic-info-config.js`.

3. Run the development environment.

   ```bash
   npm run serve
   ```

4. Package the project.

   ```bash
   npm run build
   ```

5. Check and automatically repair the code with ESLint.

   ```bash
   npm run lint
   ```

## Development Description

Technology stack: Vue.js 2 + Pug + Stylus

UI component library: element-ui

1. Import the element-ui component in TUIPlayer as needed. To add this component, confirm whether it is imported in the `src/main.js` file.

2. In TUIPlayer, use `#006EFF` as the theme color of element-ui. For more information on how to change the theme color, see [Custom theme](https://element.eleme.cn/#/en-US/component/custom-theme).

3. Import icons 

   1) TUIPlayer manages .svg icons in the `src/assets/icons/svg` path in a unified manner.

   2) Use the global component svg-icon to load the .svg icons. Note that the values bound to `icon-name` are the same as the .svg icon filenames in the `src/assets/icons/svg` path.

   ```pug
   svg-icon(icon-name="info")
   ```

   3) You can add a style to modify the .svg icon color and size.

   ```pug
   svg-icon.icon-style(icon-name="info")
   ```

   ```stylus
   .icon-style
     fill #000000
     width 16px
     height 16px
   ```