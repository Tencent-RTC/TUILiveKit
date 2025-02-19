import { isNeedLogin } from '../utils/constants';
import { isMobile } from '@tencentcloud/livekit-web-vue3/es/utils/environment';

class VConsoleOutputLogsPlugin {
  constructor(vConsole) {
    this.vConsole = vConsole;
    this.$ = vConsole.$;
    this.dom = null;
    return this.init();
  }

  init() {
    const vConsoleExportLogs = new window.VConsole.VConsolePlugin('exportLog');

    vConsoleExportLogs.on('ready', () => {
      console.log('[vConsole-exportlog-plugin] -- load');
    });
    vConsoleExportLogs.on('addTool', (callback) => {
      const buttons = [
        {
          name: 'exportLogs',
          global: true,
          onClick: this.export,
        },
        {
          name: 'copyLogs',
          global: true,
          onClick: this.copyText,
        },
      ];
      callback(buttons);
    });
    this.vConsole.addPlugin(vConsoleExportLogs);
    return vConsoleExportLogs;
  }
  funDownload = (content, filename) => {
    const eleLink = document.createElement('a');
    eleLink.download = filename;
    eleLink.style.display = 'none';
    const blob = new Blob([content]);
    eleLink.href = URL.createObjectURL(blob);
    document.body.appendChild(eleLink);
    eleLink.click();
    document.body.removeChild(eleLink);
  };
  export = () => {
    const nodeArr = document.querySelectorAll('.vc-content .vc-plugin-content')[0].children;
    let str = '';
    for (let i = 0; i < nodeArr.length; i++) {
      const ele = nodeArr[i];
      str += `${ele.textContent.trim()}\n`;
    }
    this.funDownload(str, `${`${new Date().toLocaleDateString()} ${new Date().toLocaleTimeString()}`}.log`);
  };
  copyText = () => {
    const nodeArr = document.querySelectorAll('.vc-content .vc-plugin-content')[0].children;
    let str = '';
    for (let i = 0; i < nodeArr.length; i++) {
      const ele = nodeArr[i];
      str += `${ele.textContent.trim()}\n`;
    }
    navigator.clipboard.writeText(str);
  };
}

if (isMobile && !isNeedLogin && !window.VConsole) {
  const script = document.createElement('script');
  script.type = 'text/javascript';
  script.src = 'https://unpkg.com/vconsole@3.14.7/dist/vconsole.min.js';
  document.getElementsByTagName('head')[0].appendChild(script);
  script.onload = () => {
    const vConsole = new window.VConsole();
    new VConsoleOutputLogsPlugin(vConsole);
  };
}
