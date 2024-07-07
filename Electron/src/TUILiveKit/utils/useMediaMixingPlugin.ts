import TUIMediaMixingPlugin from '@tencentcloud/tuiroom-engine-electron/plugins/media-mixing-plugin';
import { isMainWindow, getAppPath } from './envUtils';

export { TUIMediaSourceType }  from '@tencentcloud/tuiroom-engine-electron/plugins/media-mixing-plugin';

const logger = console;

const mediaMixingPlugin = new TUIMediaMixingPlugin();
(window as any)._mediaMixingPlugin = mediaMixingPlugin;

async function initMediaServer() {
  const isPackaged = location.href.indexOf("file") === 0;
  const resourcesPath = (globalThis as any).process.resourcesPath;
  let mediaServerPath = '';
  if (isPackaged) {
    mediaServerPath = (window as any).path.join(resourcesPath, '/liteav_media_server.exe');
  } else {
    // mediaServerPath = (window as any).path.join(resourcesPath, '../../../trtc-electron-sdk/build/Release/liteav_media_server.exe');  
    const appPath = await getAppPath();
    mediaServerPath = (window as any).path.join(appPath, './node_modules/trtc-electron-sdk/build/Release/liteav_media_server.exe'); 
  }
  mediaMixingPlugin.setMediaServerPath(mediaServerPath);
}

(async function(){
  const isMain = await isMainWindow();
  logger.log('Main window:', isMain);
  if (isMain) {
    await initMediaServer();
  }
})();

export default function useMedisMixingPlugin() {
  return mediaMixingPlugin;
}
