import 'package:flutter/foundation.dart';

class MusicState {
  ValueNotifier<MusicInfo?> currentMusicInfo = ValueNotifier<MusicInfo?>(null);
  ValueNotifier<List<MusicInfo>> musicList = ValueNotifier<List<MusicInfo>>([]);
}

class MusicInfo {
  final int id;
  final String name;
  final String path;
  ValueNotifier<bool> isPlaying = ValueNotifier<bool>(false);
  ValueNotifier<double> pitch = ValueNotifier<double>(0.0);

  MusicInfo({
    required this.id,
    required this.name,
    required this.path,
  });
}
