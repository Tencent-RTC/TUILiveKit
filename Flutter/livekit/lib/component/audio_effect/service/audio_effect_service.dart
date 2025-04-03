
import '../../../live_stream/manager/index.dart';
import '../store/audio_effect_state.dart';

class AudioEffectService {
  final LiveController liveController;
  final AudioEffectState audioEffectState;

  const AudioEffectService({required this.liveController, required this.audioEffectState});

  void setVoiceChangerType(int type) async {
    await liveController.liveService.setVoiceChangerType(type);
    audioEffectState.changerType.value = type;
  }

  void setVoiceReverbType(int type) {
    liveController.liveService.setVoiceReverbType(type);
    audioEffectState.reverbType.value = type;
  }

  void enableVoiceEarMonitor(bool enable) {
    liveController.liveService.enableVoiceEarMonitor(enable);
    audioEffectState.enableVoiceEarMonitor.value = enable;
  }

  void setMusicVolume(int volume) {
    liveController.liveService.setAllMusicVolume(volume);
    audioEffectState.musicVolume.value = volume;
  }

  void setVoiceEarMonitorVolume(int volume) {
    liveController.liveService.setVoiceEarMonitorVolume(volume);
    audioEffectState.earMonitorVolume.value = volume;
  }

  void setVoiceVolume(int volume) {
    liveController.liveService.setVoiceCaptureVolume(volume);
    audioEffectState.voiceVolume.value = volume;
  }
}
