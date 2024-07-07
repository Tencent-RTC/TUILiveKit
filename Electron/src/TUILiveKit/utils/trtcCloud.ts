import TRTCCloud, { TRTCVideoResolution } from 'trtc-electron-sdk';

const trtcCloud  = TRTCCloud.getTRTCShareInstance();

export default trtcCloud;

type TUIVideoResolutionType = Record<string|number, {
  width: number;
  height: number;
}>;

const initResolutionMap = (): TUIVideoResolutionType => {
  const map: TUIVideoResolutionType = {};
  for (const key in TRTCVideoResolution) {
    const isValueProperty = parseInt(key, 10) >= 0;
    if (!isValueProperty) {
      const value = TRTCVideoResolution[key];
      const tmp = key.split('_');
      map[value] = {
        width: parseInt(tmp[1]),
        height: parseInt(tmp[2]),
      }
    }
  }
  return map;
}

export const resolutionMap:TUIVideoResolutionType  = initResolutionMap();
