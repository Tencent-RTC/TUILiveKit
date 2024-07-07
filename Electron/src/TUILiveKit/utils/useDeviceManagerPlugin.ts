import TUIDeviceManagerPlugin from '@tencentcloud/tuiroom-engine-electron/plugins/device-manager-plugin';

const deviceManagerPlugin = new TUIDeviceManagerPlugin();
(window as any)._deviceManagerPlugin = deviceManagerPlugin;

export default function useDeviceManagerPlugin() {
  return deviceManagerPlugin;
}
