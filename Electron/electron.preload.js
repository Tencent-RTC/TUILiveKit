console.log(`[preload] node version: ${process.versions.node}`);
console.log(`[preload] chrome version: ${process.versions.chrome}`);
console.log(`[preload] electron version: ${process.versions.electron}`);
console.log(`[preload] process.cwd(): ${process.cwd()}`);
console.log(`[preload] __dirname: ${__dirname}`);
console.log(`[preload] env.NODE_ENV: ${process.env.NODE_ENV}`);

const { ipcRenderer } = require("electron");
const path = require("path");

// 打印来自主进程的奔溃 dump 文件存放目录
ipcRenderer.on("crash-file-path", (event, args) => {
  console.warn("[preload] crash-file-path:", args);
});

ipcRenderer.on("native-window-handle", (event, args) => {
  console.log('[preload] native window id:', args);
  window.nativeWindowHandle = args;
});

ipcRenderer.on('login', (event, { from }) => {
  console.log(`[preload] login from:${from} window`);
  if (from === 'main') {
    if (window.location.hash.indexOf('child-window') === -1) {
      window.location.hash = 'child-window';
    }
  } else {
    // from === 'child'
    if (window.location.hash.indexOf('main-window') === -1) {
      window.location.hash = 'main-window';
    }
  }
});

ipcRenderer.on('port-to-child', (event) => {
  window.mainWindowPort = event.ports[0];
  console.log(`[preload] port-to-child window:`, window.mainWindowPort);
});

window.ipcRenderer = ipcRenderer;

window.path = path;
window.ROOT_PATH = path.join(__dirname, "../");
window.PUBLIC_PATH = path.join(__dirname);
ipcRenderer.on("app-path", (event, appPath) => {
  console.warn("APP_PATH:", appPath);
  window.APP_PATH = appPath;
});
