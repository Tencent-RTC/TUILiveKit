import 'package:tencent_live_uikit/common/language/gen/livekit_localizations.dart';
import 'package:tencent_live_uikit/common/widget/global.dart';
import 'package:tencent_trtc_cloud/trtc_cloud_def.dart';

import '../../../live_stream/manager/index.dart';
import '../store/music_state.dart';

class MusicService {
  late LiveController liveController;
  late MusicState musicState;

  MusicService({
    required this.liveController,
    required this.musicState,
  }) {
    _initData();
  }

  void operatePlayMusic(MusicInfo musicInfo) {
    final MusicInfo? currentMusicInfo = musicState.currentMusicInfo.value;
    if (currentMusicInfo?.isPlaying.value == true) {
      _stopMusic(currentMusicInfo!);
    }
    _startMusic(musicInfo);
  }

  void operateStopMusic(MusicInfo musicInfo) {
    if (musicState.currentMusicInfo.value == musicInfo) {
      musicState.currentMusicInfo.value = null;
    }
    _stopMusic(musicInfo);
  }

  void operateDeleteMusic(MusicInfo musicInfo) {
    if (musicState.currentMusicInfo.value == musicInfo) {
      musicState.currentMusicInfo.value = null;
    }
    if (musicInfo.isPlaying.value) {
      _stopMusic(musicInfo);
    }
    _deleteMusic(musicInfo);
  }
}

extension MusicServiceLogicExtension on MusicService {
  void _startMusic(MusicInfo musicInfo) {
    musicState.currentMusicInfo.value = musicInfo;
    musicInfo.isPlaying.value = true;
    final AudioMusicParam musicParam =
        AudioMusicParam(path: musicInfo.path, id: musicInfo.id, publish: true, loopCount: 100000000);
    liveController.liveService.startPlayMusic(musicParam);
  }

  void _stopMusic(MusicInfo musicInfo) {
    musicInfo.isPlaying.value = false;
    liveController.liveService.stopPlayMusic(musicInfo.id);
  }

  void _deleteMusic(MusicInfo musicInfo) {
    final List<MusicInfo> list = [];
    list.addAll(musicState.musicList.value);
    list.remove(musicInfo);
    musicState.musicList.value = list;
  }

  void _initData() {
    if (musicState.musicList.value.isEmpty) {
      List<MusicInfo> list = [];

      list.add(MusicInfo(
          id: 1,
          name: LiveKitLocalizations.of(Global.appContext())!.live_music_cheerful,
          path: "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/PositiveHappyAdvertising.mp3"));
      list.add(MusicInfo(
          id: 2,
          name: LiveKitLocalizations.of(Global.appContext())!.live_music_melancholy,
          path: "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/SadCinematicPiano.mp3"));
      list.add(MusicInfo(
          id: 3,
          name: LiveKitLocalizations.of(Global.appContext())!.live_music_wonder_world,
          path: "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/WonderWorld.mp3"));
      musicState.musicList.value = list;
    }
  }
}
