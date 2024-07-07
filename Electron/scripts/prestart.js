const fs = require("fs");
const path = require("path");
const { arch, platform } = process;

function rsyncQTDependency() {
  let sourcePath = "";
  if (platform === "win32") {
    // windows
    if (arch === "x64") {
      sourcePath = path.resolve(
        __dirname,
        "../node_modules/trtc-electron-plugin-xmagic/plugin/XMagic/win/x64/platforms/qwindows.dll"
      );
    } else if (arch === "ia32") {
      sourcePath = path.resolve(
        __dirname,
        "../node_modules/trtc-electron-plugin-xmagic/plugin/XMagic/win/ia32/platforms/qwindows.dll"
      );
    } else {
      console.error("not supported arch:", arch);
      return;
    }
    rsyncOnWindows(sourcePath);
  } else if (platform === "darwin") {
    // mac os
  } else if (platform === "linux") {
    // linux
  } else {
    console.error(
      `Platform "${platform}" is supported by trtc-electron-sdk and the beauty library.`
    );
  }
}

function rsyncOnWindows(sourcePath) {
  const electronFolder = path.resolve(
    __dirname,
    "../node_modules/electron/dist/"
  );
  const targetFolder = path.join(electronFolder, "/platforms");
  const targetPath = path.join(targetFolder, "/qwindows.dll");

  console.log("source:", sourcePath);
  console.log("target:", targetPath);

  if (fs.existsSync(electronFolder)) {
    if (!fs.existsSync(targetPath)) {
      fs.mkdirSync(targetFolder, { recursive: true });
      fs.copyFileSync(sourcePath, targetPath);
    } else {
      // 已经存在，不用同步
      console.log("already exist, not need rsync");
    }
  } else {
    console.error(
      "Electron not install correctly, make sure to install electron firstly"
    );
  }
}

rsyncQTDependency();
