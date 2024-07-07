import TUIAudioEffectManager from "@tencentcloud/tuiroom-engine-electron/plugins/audio-effect-manager";

export { TUIVoiceReverbType, TUIVoiceChangerType, TUIAudioMusicParam } from "@tencentcloud/tuiroom-engine-electron/plugins/audio-effect-manager";

(window as any)._audioEffectManager = TUIAudioEffectManager;

export default function useAudioEffectManagerPlugin() {
  return TUIAudioEffectManager;
}