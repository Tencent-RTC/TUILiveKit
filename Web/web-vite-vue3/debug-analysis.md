# `live/demos/web-vite-vue3` 生产环境 Chat 相关崩溃根因分析

> 数据来源：本目录下 `debug.log`（部署地址 `https://web.sdk.qcloud.com/hybrid/live/vue3/dev/`）。
> 结论一句话：**不是 `@tencentcloud/lite-chat` 的版本报错，而是 `lite-chat` 插件在生产构建下没被注册到 `chat` 实例上，导致后续 `TUIChatEngine.login()` 走到 `chat.getConversationList()` 时报「is not a function」。**

---

## 1. 现象

日志末尾出现致命错误：

```
index-e-DkuUjI.js:6 TUIChatEngine.resetStore ok.
index-e-DkuUjI.js:6 Uncaught (in promise) TypeError: e.chat.getConversationList is not a function
    at An.getConversationInitData (index-e-DkuUjI.js:6)
    at An.init (index-e-DkuUjI.js:6)
    at Ps.initService (index-e-DkuUjI.js:6)
    at Ps.login (index-e-DkuUjI.js:6)
    at Un.login (index-e-DkuUjI.js:6)
```

调用栈还原（对应 `chat/packages/chat-uikit-engine-lite/src`）：

- `Un.login` = `TUIChatEngine.login({ chat, SDKAppID, userID, userSig })`
- `Ps.login` → `Ps.initService` → `TUIConversationService.init()`
- `An.getConversationInitData` → `chat.getConversationList()` ← 抛错

关键日志上下文（按时间顺序）：

| 行 | 打印 | 说明 |
|---|---|---|
| 1 | `roomEngine-*.js: TencentCloudLiteChat.VERSION:1.6.16` | Room 分包附带的 lite-chat（1.6.16） |
| 5–6 | `index-B_rX16np.js: TencentCloudLiteChat.VERSION:1.6.16` / `TUICore-Lite.VERSION:1.0.1` | 主 chunk 加载 lite-chat + tui-core-lite |
| 14 | `LiteChat ... [connect]` | `TUILogin.login()` 内部启动 `chat.login()` |
| 181 | `[login] login success 1 ... href: .../live-list` | `TUILogin.login()` 成功 |
| 182 | `TUIEventManager.notifyEvent ... userLoginSuccess` | tui-core-lite 派发登录成功事件 |
| 221 | `index-e-DkuUjI.js: TUIChatEngine-Lite.VERSION:1.0.7` | **chat-uikit-engine-lite 在这一刻才被首次加载**（异步 chunk） |
| 226 | `TUIEventManager.registerEvent ... userLoginSuccess` + `renotify success` | 新加载的 subEntry 在 `TUICore.registerEvent` 时触发 `renotify`，把上一条 `userLoginSuccess` 补投给自己 |
| 258–278 | `TUIChatEngine.resetStore ok.` → `TypeError: e.chat.getConversationList is not a function` | 补投触发 `RTCLoginServer.login()` → `TUIChatEngine.login(...)` → 崩溃 |

版本一览（都是彼此兼容的当前版本，**没有版本错配问题**）：

- `@tencentcloud/lite-chat`: 1.6.16
- `@tencentcloud/chat-uikit-engine-lite`: 1.0.7
- `@tencentcloud/tui-core-lite`: 1.0.1
- `@tencentcloud/tuiroom-engine-js`: 4.2.1
- `@tencentcloud/universal-api`: 2.4.1

## 2. 关键代码链路

### 2.1 `TUILogin.login` 依赖的“插件白板”

`base/packages/tuicore-lite/src/TUILogin/tui-login.ts`：

```ts
this.chat = TencentCloudChat.create(loginParams); // 基于 @tencentcloud/lite-chat/basic
...
this._addSDKExtensions(); // 内部执行 this.chat.use(APP_NAMESPACE.__$RTC_CHAT__)
```

也就是说 `TUILogin` 使用的是 **`lite-chat/basic`**（不含任何插件的基础包），登录时必须依赖一个全局白板 `window.__$RTC_CHAT__` 来批量注册插件（`conversation`、`group`、`avchatroom`、`friend`、`signaling`、`rich-media-message`、`cloud-search`、`message-enhancer`、`conversation-group`）。缺少 `conversation` 插件时，`chat.getConversationList()` 就不是函数，恰好命中日志中的报错。

### 2.2 谁负责给 `__$RTC_CHAT__` 塞插件

项目里有两条互补链路：

1. `chat/packages/chat-uikit-engine-lite/src/TUIEngine/engine.ts` 里 `ChatEngine.getInstance()`：

   ```ts
   APP_NAMESPACE.__$RTC_CHAT__ = [
     conversationPlugin, groupPlugin, avChatRoomPlugin,
     messageEnhancerPlugin, richMediaMessagePlugin,
     cloudSearchPlugin, signalingPlugin, friendPlugin,
     conversationGroupPlugin,
   ];
   ```

   而 `chat-uikit-engine-lite/src/index.ts` 顶层就 `const TUIChatEngine = ChatEngine.getInstance();`，只要该模块被 evaluate，插件白板即被填好。

2. `base/packages/atomicxcore/packages/core/src/stores/LoginStore/LoginStore.ts` 顶层：

   ```ts
   const g = (typeof window !== 'undefined' ? window : globalThis) as any;
   if (!Array.isArray(g.__$RTC_CHAT__)) g.__$RTC_CHAT__ = [];
   for (const plugin of REQUIRED_PLUGINS) {
     if (!g.__$RTC_CHAT__.includes(plugin)) g.__$RTC_CHAT__.push(plugin);
   }
   ```

   `atomicxcore/core` 只要被引入即完成注册。

任何一条链路**只要在 `TUILogin.login()` 之前 evaluate**，`_addSDKExtensions` 就能拿到插件；否则 `chat.use([])` 是空调用，插件永远不会被挂上。

### 2.3 Demo 的 `useLoginState` 走的是哪条路径

`live/demos/web-vite-vue3/src/views/login.vue`：

```ts
import { useLoginState, UIKitModal } from 'tuikit-atomicx-vue3';
const { login } = useLoginState();
await login({ ... }); // 内部会调 TUILogin.login()
```

`tuikit-atomicx-vue3` 主入口（`ui-component/packages/uikit-component-vue3/src/index.ts`）里：

```ts
export * from './subEntry/common';        // 包含 useLoginState（走 uikit-core）
export * from './subEntry/chat/chat';     // 只是聚合导出 chat 组件 / states
export * from './chat-store';             // 从 @atomicxcore/core 导出 LoginStore 等
export * from './subEntry/live/live';     // 只是聚合导出 live 组件 / states
export * from './subEntry/room/room';     // 只是聚合导出 room 组件 / states
```

**注意 3 个 `subEntry/*/index.ts`（真正带副作用：`ChatLoginServer.getInstance().init()` / `RTCLoginServer.getInstance().init()`）并没有被主入口导入，只在 `tuikit-atomicx-vue3/{chat,live,room}` 子入口才会执行**。demo 全量使用的是主入口 `'tuikit-atomicx-vue3'`。

`useLoginState` 的实现（`ui-component/packages/uikit-component-vue3/src/states/LoginState/index.ts`）来自 `uikit-core/states/LoginState/store.ts`：内部 `await TUILogin.login(...)`，**并没有触发 `chat-uikit-engine-lite` 或 `@atomicxcore/core` 的 evaluate**。

## 3. 为什么 dev OK，prod 崩

### 3.1 dev（`vite dev`）

- Vite 不做 tree-shaking / code-splitting；模块基于原生 ESM，按 `import` 静态图逐个 fetch。
- `App.vue → 路由 → main.ts` 静态图会牵扯到 `LivePlayer / LiveList / BarrageList / ConversationListState` 等，这些文件里有：

  ```ts
  import TUIChatEngine, { StoreName, TUIStore, ... } from '@tencentcloud/chat-uikit-engine-lite';
  ```

  等**值型** import，浏览器在页面初次加载就会 fetch 这些模块并 evaluate。
- 因此 `chat-uikit-engine-lite/src/index.ts` 的顶层 `ChatEngine.getInstance()` **在用户点击登录前就完成**了，`window.__$RTC_CHAT__` 被填好。
- 随后 `TUILogin.login()` 里的 `_addSDKExtensions()` 成功注册插件，`chat.getConversationList()` 正常。

### 3.2 prod（`vite build`）

`vite.config.ts`：

```ts
build: {
  rollupOptions: {
    output: {
      manualChunks: {
        roomEngine: ['@tencentcloud/tuiroom-engine-js'],
      },
    },
  },
},
```

Rollup 打包时：

1. `login.vue` / `router` / `main.ts` 组成的**首屏可达集合**中，只用了 `useLoginState / UIKitModal / Login(LoginModel) / useUIKit / addI18n`。**首屏并不静态依赖任何 chat state / BarrageList**（那些组件位于 `live-list-*.js`、`live-player-*.js`、`live-pusher-*.js`、`business-live-player-*.js` 等**懒加载 route chunk**）。
2. `@tencentcloud/chat-uikit-engine-lite` 的 `package.json` 里 **未声明 `"sideEffects": false`**，但 Rollup 依然按 `import` 图定位；由于所有对它的引用都在 lazy route chunk 内，Rollup 就把 `chat-uikit-engine-lite` 切进一个共享懒 chunk（本次日志里叫 `index-e-DkuUjI.js`）。
3. `@atomicxcore/core` 的 `LoginStore` 只通过 `tuikit-atomicx-vue3` 的 `chat-store/composables/useLoginStore.ts` 引入（`import { LoginStore } from '@atomicxcore/core';`），而**登录页并没有使用 `useLoginStore`**。加上 `uikit-component-vue3` 自己的 `vite.config.ts` 明确声明：

   ```ts
   treeshake: {
     preset: 'smallest',
     moduleSideEffects: (id) => {
       if (id.includes('@tencentcloud/chat-uikit-engine-lite') ||
           id.includes('@tencentcloud/tuiroom-engine-js')) return false;
       if (id.endsWith('.css')) return true;
       return false; // <-- 默认认为其它模块没有副作用
     },
   },
   ```

   → `tuikit-atomicx-vue3` 本身的 dist 已经把 `chat-uikit-engine-lite` 视为“无副作用”，允许下游继续 tree-shake；`useLoginStore` 未被使用时，其顶层 `import { LoginStore } from '@atomicxcore/core'` 也可被 Rollup 判为死代码，`__$RTC_CHAT__` 注入也就随之丢失。

于是首屏可达图里 **既没有 `chat-uikit-engine-lite` 的顶层副作用，也没有 `atomicxcore/LoginStore` 的顶层副作用**，`window.__$RTC_CHAT__` 保持为 `undefined`。

后续时序（对应 debug.log）：

```
[登录页首屏]
  useLoginState().login()
    → TUILogin.login()
      → new lite-chat/basic 实例
      → _addSDKExtensions()：读 __$RTC_CHAT__ → undefined → chat.use([]) → 没注册任何插件
    → chat.login({ userID, userSig }) 成功
  → USER_LOGIN_SUCCESS 事件派发（uikit-core LoginState 的 handler 只更新状态，不会走 TUIChatEngine.login）

[路由跳转 /live-list，懒加载 index-e-*.js]
  chat-uikit-engine-lite 模块 evaluate  ← 打印 "TUIChatEngine-Lite.VERSION:1.0.7"
    顶层执行 ChatEngine.getInstance() → 现在才把 9 个插件塞进 __$RTC_CHAT__（**太晚**）
  subEntry/live/index.ts 也在这个 chunk 里 evaluate
    RTCLoginServer.getInstance().init() → TUICore.registerEvent(USER_LOGIN_SUCCESS, this)
    tui-core-lite 的 renotify 机制立刻把已发生过的 USER_LOGIN_SUCCESS 补投给它
  RTCLoginServer.onNotifyEvent(USER_LOGIN_SUCCESS)
    → this.login()
      → 从 TUILogin.getContext() 拿到「插件缺失的 chat 实例」
      → TUIChatEngine.login({ chat, SDKAppID, userID, userSig })
        → 因为 chat 已 isReady，直接走 initService()
          → TUIConversation.init() → getConversationInitData() → chat.getConversationList()
          → chat 上根本没有 getConversationList → 💥 TypeError
```

### 3.3 「为什么 `roomEngine-*` 里那份 `TencentCloudLiteChat.VERSION:1.6.16` 没解决问题」

`roomEngine-*` chunk 是 `manualChunks` 单独切出去的 `TUIRoomEngine`。`TUIRoomEngine` 内部依赖 lite-chat/basic 只是为了自己使用（例如 `TUIRoomEngine.login({ tim: chat })`），并不会去动 `APP_NAMESPACE.__$RTC_CHAT__`。所以它先执行只是把 lite-chat 模块 evaluate 一次（打印一次 VERSION），对 UIKit 侧插件注册没有帮助。

## 4. 根因结论

1. **登录时 `TUILogin.login()` 依赖全局 `window.__$RTC_CHAT__` 中的 lite-chat 插件白板**，而这个白板由 `chat-uikit-engine-lite` 或 `@atomicxcore/core/LoginStore` 的**模块顶层副作用**填充。
2. `live/demos/web-vite-vue3` 通过 `useLoginState('tuikit-atomicx-vue3')` 登录时，**没有任何静态 import 链路能在登录页首屏把上述副作用带进来**（本 demo 的登录页只用 `useLoginState / UIKitModal / addI18n`，都是 uikit-core / 通用模块，不牵扯 chat 引擎）。
3. dev 环境无 tree-shaking + 无 code-splitting，chat 引擎在首屏就被 evaluate，掩盖了这一时序问题；prod 环境 Rollup 把 chat 引擎切进懒 chunk，`_addSDKExtensions()` 只能读到空白板，插件全部缺失。
4. 后续访问直播列表页时 chat 引擎才被懒加载并通过 tui-core-lite 的 `renotify` 补投登录事件，进入 `TUIChatEngine.login()` 时命中 `chat.getConversationList is not a function`。

**这不是 `lite-chat` 的版本报错，而是打包/加载时序导致的插件未注册问题。**

## 5. 修复方案

按“改动范围从小到大”排序，推荐前两个之一。

### 方案 A（最小改动，Demo 侧）：登录前显式导入 chat 子入口

在 `live/demos/web-vite-vue3/src/main.ts` 顶部加上：

```ts
// Ensure lite-chat plugins are registered before TUILogin.login() runs.
// Importing the chat sub-entry triggers ChatEngine.getInstance() which
// populates window.__$RTC_CHAT__, otherwise Rollup tree-shakes chat
// engine into a lazy chunk and TUILogin.login() gets an empty plugin
// list in production build.
import 'tuikit-atomicx-vue3/chat';
```

原理：`tuikit-atomicx-vue3/chat` 的入口 `subEntry/chat/index.ts` 有明确的顶层副作用（`ChatLoginServer.getInstance().init()`），并 `export * from './chat'` 拉起 `chat-uikit-engine-lite`。这样即便下游不用 chat 组件，也一定会在登录之前把插件白板准备好。

- 开发成本：1 行代码
- 包体积：会把 chat states / 组件常量拉进首屏（部分组件仍会按需 lazy），可接受
- 兼容性：与现有登录、直播流程无冲突

### 方案 B（Demo 侧更精简）：只强制导入 chat engine 模块

如果不希望连 `ChatLoginServer` 也一起进来，可以在 `main.ts` 顶部改成：

```ts
// Force-eval chat-uikit-engine-lite so its module-level side effect
// (populating window.__$RTC_CHAT__ with lite-chat plugins) happens
// before TUILogin.login() executes.
import '@tencentcloud/chat-uikit-engine-lite';
```

比方案 A 更轻，唯一副作用就是把插件塞到全局白板。

### 方案 C（治本，UIKit 侧）：`useLoginState` 内部保证插件先注册

修改 `ui-component/packages/uikit-core/src/states/LoginState/store.ts` 的 `login()`：在 `await TUILogin.login(loginParams)` 之前先动态加载 chat 引擎，例如：

```ts
export async function login(options: LoginParams): Promise<void> {
  // Ensure lite-chat plugins are pre-registered on the global plugin
  // registry before TUILogin.login() reads it via _addSDKExtensions().
  await import('@tencentcloud/chat-uikit-engine-lite');
  // ...existing code
}
```

优点：所有基于 `useLoginState` 的接入方（chat/live/room demo）自动生效。
缺点：Call FT 等不需要 chat 插件的接入方也会被强制拉一份 chat engine（~100KB）。建议按 `login({ engines: ['chat'|'live'|'room'] })` 参数化开关，参照 `doc/tech-design/tyrosning/atomicx-chat-sdk-plugin-registration-issue.md` 中的方案五。

### 方案 D（Demo 打包侧兜底）：显式声明 chat engine 有副作用

若坚持不改源代码，只调整 demo 的 `vite.config.ts`：

```ts
build: {
  commonjsOptions: { transformMixedEsModules: true },
  rollupOptions: {
    output: { manualChunks: { roomEngine: [...] } },
    // Force keep module-level side effects of chat engine.
    treeshake: {
      moduleSideEffects: (id) =>
        id.includes('@tencentcloud/chat-uikit-engine-lite')
        || id.includes('@atomicxcore/core')
        || id.endsWith('.css'),
    },
  },
},
```

不推荐单独使用：因为 `tuikit-atomicx-vue3` 内部已经把 chat engine 视为无副作用，靠下游关掉 tree-shake 有点脆弱；但可与方案 A/B 一起作为“双保险”。

## 6. 验证方式

1. 在 `login.vue` 或 `main.ts` 应用方案 A/B。
2. `npm run build && npm run preview`，Console 期望：
   - 登录成功前打印 `TUIChatEngine-Lite.VERSION:1.0.7`（说明 chat engine 已 evaluate）；
   - `TUIEventManager.registerEvent ... userLoginSuccess` 应出现在 `TUILogin.login` 完成前；
   - 进入 `/live-list` 时不再抛 `getConversationList is not a function`；
   - `chat.callExperimentalAPI('canIUseModule', ['grp'])` 等接口正常。
3. 无痕窗口首次登录 + 挤下线后原地重登两种路径都跑一遍，确保时序回归。

## 7. 关联文档

- `doc/tech-design/tyrosning/atomicx-chat-sdk-plugin-registration-issue.md`：本项目对同类问题（Chat 场景）的详细预案，本次直播场景表现完全一致。
- `doc/tech-design/icebergfeng/【LiveKit】【Vue3 Demo】直播列表拉取失败 not-inited Bug 分析.md`：另一个直播列表相关问题，与本次问题无关但同样是登录→列表时序引起。
