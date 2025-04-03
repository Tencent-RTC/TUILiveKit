import 'package:tencent_trtc_cloud/trtc_cloud.dart';
import 'package:tencent_trtc_cloud/tx_audio_effect_manager.dart';

import '../state/audio_effect_state.dart';
import '../state/audio_effect_state_factory.dart';

class AudioEffectManager {
  final String roomId;
  final AudioEffectState state;
  late final TRTCCloud? trtcCloud;
  late final TXAudioEffectManager? audioEffectManager;

  AudioEffectManager({required this.roomId, required this.state}) {
    _createAudioEffectManager();
    _subscribeStateRemoval();
  }

  void setVoiceChangerType(int type) async {
    await audioEffectManager?.setVoiceChangerType(type);
    state.changerType.value = type;
  }

  void setVoiceReverbType(int type) {
    audioEffectManager?.setVoiceReverbType(type);
    state.reverbType.value = type;
  }

  void enableVoiceEarMonitor(bool enable) {
    audioEffectManager?.enableVoiceEarMonitor(enable);
    state.enableVoiceEarMonitor.value = enable;
  }

  void setMusicVolume(int volume) {
    audioEffectManager?.setAllMusicVolume(volume);
    state.musicVolume.value = volume;
  }

  void setVoiceEarMonitorVolume(int volume) {
    audioEffectManager?.setVoiceEarMonitorVolume(volume);
    state.earMonitorVolume.value = volume;
  }

  void setVoiceVolume(int volume) {
    audioEffectManager?.setVoiceCaptureVolume(volume);
    state.voiceVolume.value = volume;
  }
}

extension on AudioEffectManager {
  void _createAudioEffectManager() async {
    trtcCloud = await TRTCCloud.sharedInstance();
    audioEffectManager = trtcCloud?.getAudioEffectManager();
  }

  void _subscribeStateRemoval() {
    AudioEffectStateFactory.addRemovalListener(roomId, _handleStateRemoved);
  }

  void _handleStateRemoved() {
    _dispose();
  }

  void _dispose() {
    _resetAudioSettings();
    AudioEffectStateFactory.removeRemovalListener(roomId);
  }

  void _resetAudioSettings() {
    audioEffectManager?.enableVoiceEarMonitor(false);
    audioEffectManager?.setVoiceEarMonitorVolume(100);

    audioEffectManager?.setAllMusicVolume(60);
    audioEffectManager?.setVoiceCaptureVolume(100);

    audioEffectManager?.setVoiceChangerType(0);
    audioEffectManager?.setVoiceReverbType(0);
  }
}
