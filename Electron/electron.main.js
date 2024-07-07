const {
  app,
  BrowserWindow,
  systemPreferences,
  crashReporter,
  ipcMain,
  Menu,
  // screen,
} = require("electron");
const path = require("path");

// 开启crash捕获
crashReporter.start({
  productName: "tui-live-kit-electron",
  companyName: "Tencent Cloud",
  submitURL: "https://www.xxx.com",
  uploadToServer: false,
  ignoreSystemCrashHandler: false,
});

// 开启crash捕获
let crashFilePath = "";
let crashDumpsDir = "";
try {
  // electron 低版本
  crashFilePath = path.join(app.getPath("temp"), app.getName() + " Crashes");
  console.log("————————crash path:", crashFilePath);

  // electron 高版本
  crashDumpsDir = app.getPath("crashDumps");
  console.log("————————crashDumpsDir:", crashDumpsDir);
} catch (e) {
  console.error("获取奔溃文件路径失败", e);
}

async function checkAndApplyDeviceAccessPrivilege() {
  if (process.platform === "darwin" || process.platform === 'win32') {
    try {
      const cameraPrivilege = systemPreferences.getMediaAccessStatus("camera");
      console.log(
        `checkAndApplyDeviceAccessPrivilege before apply cameraPrivilege: ${cameraPrivilege}`
      );
      if (cameraPrivilege !== "granted") {
        await systemPreferences.askForMediaAccess("camera");
      }
  
      const micPrivilege = systemPreferences.getMediaAccessStatus("microphone");
      console.log(
        `checkAndApplyDeviceAccessPrivilege before apply micPrivilege: ${micPrivilege}`
      );
      if (micPrivilege !== "granted") {
        await systemPreferences.askForMediaAccess("microphone");
      }
  
      const screenPrivilege = systemPreferences.getMediaAccessStatus("screen");
      console.log(
        `checkAndApplyDeviceAccessPrivilege before apply screenPrivilege: ${screenPrivilege}`
      );
    } catch(err) {
      console.warn("checkAndApplyDeviceAccessPrivilege error:", err)
    }
  } else {
    // Electron API getMediaAccessStatus/askForMediaAccess does not support on Linux.
  }
}

const windowMap = {
  main: null,
  child: null
}

let language = "zh-CN";

function isZhCN() {
  return language === "zh-CN";
}

async function createWindow(width = 1366, height = 668) {
  await checkAndApplyDeviceAccessPrivilege();

  windowMap.main = new BrowserWindow({
    width: 1200 || width,
    height: 650 || height,
    minWidth: 1200,
    minHeight: 650,
    frame: false,
    acceptFirstMouse: true, // only mac
    webPreferences: {
      preload: path.join(__dirname, "electron.preload.js"),
      nodeIntegration: true,
      contextIsolation: false,
      nodeIntegrationInWorker: true,
    },
  });
  windowMap.child = new BrowserWindow({
    show: false,
    width: 600,
    height: 650,
    // parent: windowMap.main, // To do: 父子窗口在 Windows 和 Mac 上一致，暂时注释掉，后续确定不可行时，再删除。
    frame: false,
    acceptFirstMouse: true, // only mac
    skitTaskbar: true,
    resizable: false,
    webPreferences: {
      nodeIntegration: true,
      nodeIntegrationInWorker: true,
      contextIsolation: false,
      preload: path.join(__dirname, "electron.preload.js"),
    },
  });

  bindIPCEvent();
  bindMainWindowEvent();
  bindChildWindowEvent();  

  if (app.isPackaged) {
    windowMap.main.loadFile("dist/index.html");
    windowMap.child.loadFile("dist/index.html");
  } else {
    windowMap.child.loadURL("http://localhost:8080");
    windowMap.main.loadURL("http://localhost:8080");
  }
}

function bindIPCEvent() {
  ipcMain.handle("app-path", () => {
    return app.getAppPath();
  });

  ipcMain.handle("window-type", (event) => {
    if (event.sender === windowMap.main.webContents) {
      return 'main';
    } else if (event.sender === windowMap.child.webContents) {
      return 'child';
    } else {
      return '';
    }
  });
  
  ipcMain.on("on-minimize-window", () => {
    console.log("on-minimize-window event");
    windowMap.main.minimize();
  });

  ipcMain.on("on-maximize-window", (evt, flag) => {
    console.log("on-maximize-window event:", flag);
    if (flag) {
      windowMap.main.maximize();
    } else {
      windowMap.main.unmaximize();
    }
  });

  ipcMain.on("on-close-window", () => {
    console.log("on-close-window event");
    windowMap.main.close();
    windowMap.child.close();
  });

  ipcMain.on("open-child", (event, args) => {
    console.log("on open-child", args);
    const [width, height] = windowMap.main.getSize();
    switch (args.command) {
    case 'camera':
      windowMap.child.setSize(600, 650, true);
      windowMap.child.setContentSize(600, 650, true);
      break;
    case 'image':
      windowMap.child.setSize(600, 500, true);
      windowMap.child.setContentSize(600, 500, true);
      break;
    case 'screen':
      windowMap.child.setSize(width - 150, height - 80, true);
      windowMap.child.setContentSize(width -150, height - 80, true);
      break;
    case 'voice-chat':
    case 'setting':
      windowMap.child.setSize(600, 560, true);
      windowMap.child.setContentSize(600, 560, true);
      break;
    default:
      break;
    }
    windowMap.child.center();
    windowMap.child.show();
    windowMap.child.webContents.send("show", args);
  });

  ipcMain.on("close-child", () => {
    windowMap.child.hide();
  });

  ipcMain.on("login", (event) => {
    if (event.sender === windowMap.main.webContents) {
      windowMap.child.webContents.send("login", { from: 'main' });
    } else {
      windowMap.main.webContents.send("login", { from: 'child' });
    }
  });

  ipcMain.on("port-to-child", (event) => {
    const port = event.ports[0];
    console.log("port-to-child", port);
    windowMap.child.webContents.postMessage("port-to-child", null, [port]);
  });

  ipcMain.on("set-language", (event, args) => {
    console.log("set-language", args);
    language = args;
  });

  ipcMain.on("show-context-menu", (event) => {
    const template = [
      {
        label: isZhCN() ? "排序" : "Sort",
        submenu: [
          { "label": isZhCN() ? "上移" : "Move up", "click": () => { event.sender.send('context-menu-command', 'move-up'); } },
          { "label": isZhCN() ? "下移" : "Move down", "click": () => { event.sender.send('context-menu-command', 'move-down'); } },
          { "label": isZhCN() ? "移至顶部" : "Move to top", "click": () => {  event.sender.send('context-menu-command', 'move-top'); } },
          { "label": isZhCN() ? "移至底部" : "Move to bottom", "click": () => {  event.sender.send('context-menu-command', 'move-bottom'); }},
        ]
      },
      {
        label: isZhCN() ? "变换" : "Transform",
        submenu: [
          { "label": isZhCN() ? "顺时针旋转90度" : "Rotate 90 degrees CW", "click": () => { event.sender.send('context-menu-command', 'transform-clockwise-90'); } },
          { "label": isZhCN() ? "逆时针旋转90度" : "Rotate 90 degrees CCW", "click": () => { event.sender.send('context-menu-command', 'transform-anti-clockwise-90'); } },
          { "label": isZhCN() ? "水平旋转" : "Flip horizontal", "click": () => {  event.sender.send('context-menu-command', 'transform-mirror-horizontal'); } },
          { "label": isZhCN() ? "垂直旋转" : "Flip vertical", "click": () => {  event.sender.send('context-menu-command', 'transform-mirror-vertical'); }},
          // { type: 'separator' },
          // { "label": "还原", "click": () => {  event.sender.send('context-menu-command', 'transform-reset'); }},
        ]
      },
      { type: "separator" },
      { label: isZhCN() ? "隐藏" : "Hide", click: () => {  event.sender.send('context-menu-command', 'hide'); } },
      // { label: "锁定", click: () => {  event.sender.send('context-menu-command', 'lock'); } },
      { label: isZhCN() ? "编辑" : "Edit", click: () => {  event.sender.send('context-menu-command', 'edit'); } },
      { type: 'separator' },
      { label: isZhCN() ? "删除" : "Remove", click: () => {  event.sender.send('context-menu-command', 'remove'); } },
    ]
    const menu = Menu.buildFromTemplate(template)
    menu.popup({ window: BrowserWindow.fromWebContents(event.sender) })
  })
}

function bindMainWindowEvent() {
  windowMap.main.webContents.on("did-fail-load", () => {
    console.log(`main window: did-fail-load, reload soon...`);
    setTimeout(() => {
      windowMap.main.reload();
    }, 1000);
  });

  windowMap.main.webContents.on("did-finish-load", () => {
    windowMap.main.webContents.send("app-path", app.getAppPath());
    windowMap.main.webContents.send("crash-file-path",`${crashFilePath}|${crashDumpsDir}`);
    windowMap.main.webContents.send("native-window-handle", windowMap.main.getNativeWindowHandle());
    windowMap.main.webContents.send("window-type", "main");
  });

  windowMap.main.on("closed", () => {
    if (windowMap.child) {
      windowMap.child.close();
      windowMap.child = null;
    }
    windowMap.main = null;    
  });
}

function bindChildWindowEvent() {
  windowMap.child.webContents.on("did-fail-load", () => {
    console.log(`child window: did-fail-load, reload soon...`);
    setTimeout(() => {
      windowMap.child.reload();
    }, 2000);
  });

  windowMap.child.webContents.on('did-finish-load', function(){
    console.log('child did-finish-load');
    windowMap.child.webContents.send("app-path", app.getAppPath());
    windowMap.child.webContents.send("native-window-handle", windowMap.child.getNativeWindowHandle());
    windowMap.child.webContents.send("window-type", "child");
  });

  windowMap.child.on("close", (event) => {
    if (windowMap.main) {
      event.preventDefault();
      windowMap.child.hide();
    }  
  });

  windowMap.child.on("closed", () => {
    if (windowMap.child) {
      windowMap.child = null;
    }  
  });
}

app.whenReady().then(() => {
  const { screen } = require("electron");
  const primaryDisplay = screen.getPrimaryDisplay();
  const { width, height } = primaryDisplay.workAreaSize;
  const winWidth = width - 40;
  const winHeight = height - 40;

  if (!app.isPackaged) {
    console.log("Added Extension: installing vue-dev tool...");
    const {
      default: installExtension,
      VUEJS_DEVTOOLS,
    } = require("electron-devtools-installer");
    installExtension(VUEJS_DEVTOOLS)
      .then((name) => {
        console.log(`Added Extension:  ${name}`);
        createWindow(winWidth, winHeight);
      })
      .catch((err) => {
        console.error("Added Extension failed: ", err);
        createWindow(winWidth, winHeight);
      });
  } else {
    console.log("Packaged env, create window without dev-tool extension.");
    createWindow(winWidth, winHeight);
  }
});

// 当全部窗口关闭时退出。
app.on("window-all-closed", () => {
  // 在 macOS 上，除非用户用 Cmd + Q 确定地退出，
  // 否则绝大部分应用及其菜单栏会保持激活。
  if (process.platform !== "darwin") {
    app.quit();
  }
  process.exit(0);
});

app.on("activate", () => {
  // 在 macOS 上，当点击 dock 图标并且该应用没有打开的窗口时，
  // 绝大部分应用会重新创建一个窗口。
  if (windowMap.main === null) {
    createWindow();
  }
});

ipcMain.on('app-exit', () => {
  windowMap.child?.close();
  windowMap.main?.close();
  app.exit();
})