import TUIRoomEngine from '@tencentcloud/tuiroom-engine-electron';
import { isVue3 } from '../constants/env';
const roomEngine: Record<string, TUIRoomEngine | null> = { instance: null };
const vueVersion = isVue3 ? 'vue3' : 'vue2';
export default function useGetRoomEngine() {
  return roomEngine;
}

TUIRoomEngine.once('ready', () => {
  roomEngine.instance = new TUIRoomEngine();
  TUIRoomEngine.callExperimentalAPI(JSON.stringify({
    api: 'setFramework',
    params: {
      component: 'TUILiveKit',
      language: vueVersion,
    },
  }));
});