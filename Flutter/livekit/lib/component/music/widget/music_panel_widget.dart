import 'package:flutter/material.dart';

import '../../../common/index.dart';
import '../service/music_service.dart';

class MusicPanelWidget extends BasicWidget {
  const MusicPanelWidget({super.key, required super.liveController});

  @override
  MusicPanelWidgetState getState() {
    return MusicPanelWidgetState();
  }
}

class MusicPanelWidgetState extends BasicState<MusicPanelWidget> {
  late final MusicService musicService;

  @override
  void initState() {
    super.initState();
    _initData();
  }

  @override
  Widget build(BuildContext context) {
    return Container(
      width: screenWidth,
      height: 375,
      decoration: const BoxDecoration(
        color: LiveColors.designStandardG2,
        borderRadius: BorderRadius.only(topLeft: Radius.circular(20), topRight: Radius.circular(20)),
      ),
      child: Column(children: [_initTitleWidget(), 12.verticalSpace, _initListWidget()]),
    );
  }

  _initTitleWidget() {
    return SizedBox(
      height: 44,
      width: screenWidth,
      child: Stack(
        children: [
          Center(
            child: Text(
              LiveKitLocalizations.of(Global.appContext())!.live_music,
              style: const TextStyle(color: LiveColors.designStandardG7, fontSize: 16),
            ),
          ),
        ],
      ),
    );
  }

  Widget _initListWidget() {
    final double screenWidth = MediaQuery.of(context).size.width;
    return ValueListenableBuilder(
      valueListenable: musicService.musicState.musicList,
      builder: (BuildContext context, value, Widget? child) {
        return SizedBox(
            width: screenWidth,
            height: 375 - 56,
            child: ListView.builder(
                itemCount: musicService.musicState.musicList.value.length,
                itemExtent: 60.0,
                itemBuilder: (BuildContext context, int index) {
                  final musicInfo = musicService.musicState.musicList.value[index];
                  return Padding(
                      padding: const EdgeInsets.symmetric(horizontal: 24),
                      child: ValueListenableBuilder(
                          valueListenable: musicInfo.isPlaying,
                          builder: (BuildContext context, value, Widget? child) {
                            return SizedBox(
                                height: 60,
                                width: double.infinity,
                                child: Stack(
                                  children: [
                                    Positioned(
                                      left: 0,
                                      top: 20,
                                      child: Text(
                                        musicInfo.name,
                                        style: const TextStyle(
                                            color: LiveColors.designStandardFlowkitWhite, fontSize: 16),
                                      ),
                                    ),
                                    Positioned(
                                      right: 0,
                                      top: 22,
                                      child: GestureDetector(
                                        onTap: () {
                                          musicService.operateDeleteMusic(musicInfo);
                                        },
                                        child: Container(
                                          width: 16,
                                          height: 16,
                                          padding: const EdgeInsets.all(0),
                                          child: Image.asset(
                                            LiveImages.musicDelete,
                                            package: Constants.pluginName,
                                          ),
                                        ),
                                      ),
                                    ),
                                    Positioned(
                                      right: 40,
                                      top: 22,
                                      child: GestureDetector(
                                        onTap: () {
                                          if (musicInfo.isPlaying.value) {
                                            musicService.operateStopMusic(musicInfo);
                                          } else {
                                            musicService.operatePlayMusic(musicInfo);
                                          }
                                        },
                                        child: Container(
                                          width: 16,
                                          height: 16,
                                          padding: const EdgeInsets.all(0),
                                          child: Image.asset(
                                            musicInfo.isPlaying.value
                                                ? LiveImages.musicPause
                                                : LiveImages.musicStart,
                                            package: Constants.pluginName,
                                          ),
                                        ),
                                      ),
                                    ),
                                    Positioned(
                                      left: 0,
                                      top: 59,
                                      child: Container(
                                        width: screenWidth - 24 * 2,
                                        height: 1,
                                        color: LiveColors.notStandardBlue30Transparency,
                                      ),
                                    )
                                  ],
                                ));
                          }));
                }));
      },
    );
  }
}

extension MusicPanelWidgetStateLogicExtension on MusicPanelWidgetState {
  _initData() {
    final musicState = MusicStateFactory.getState(liveController.getRoomSate().roomId);
    musicService = MusicService(liveController: liveController, musicState: musicState);
  }
}
