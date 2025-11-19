import 'dart:async';

import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/live_stream/live_define.dart';

import '../../../manager/live_stream_manager.dart';

class PlayerMenuWidget extends StatefulWidget {
  final LiveStreamManager liveStreamManager;

  const PlayerMenuWidget(
      {super.key,
      required this.liveStreamManager});

  @override
  State<PlayerMenuWidget> createState() => _PlayerMenuWidgetState();
}

class _PlayerMenuWidgetState extends State<PlayerMenuWidget> {
  late final ValueNotifier<bool> _isPlaying;
  late final ValueNotifier<double> _volume;
  final _showVerticalVolumeSlider = ValueNotifier<bool>(false);
  late final VoidCallback _liveStatusChangedListener = _onLiveStatusChanged;
  late final StreamSubscription<void> _kickedOutSubscription;

  @override
  void initState() {
    super.initState();
    _isPlaying = ValueNotifier(!widget.liveStreamManager.mediaState.isRemoteVideoStreamPaused.value);
    _volume = ValueNotifier(widget.liveStreamManager.mediaState.currentPlayoutVolume.value.toDouble());
    widget.liveStreamManager.roomState.liveStatus.addListener(_liveStatusChangedListener);
    _kickedOutSubscription = widget.liveStreamManager.kickedOutSubject.stream.listen((_) => _handleKickedOut());
  }

  @override
  void dispose() {
    _kickedOutSubscription.cancel();
    widget.liveStreamManager.roomState.liveStatus.removeListener(_liveStatusChangedListener);
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return ValueListenableBuilder(
        valueListenable: _showVerticalVolumeSlider,
        builder: (context, showVerticalVolumeSlider, _) {
          return GestureDetector(
            onTap: () => Navigator.pop(context),
            child: Container(
              width: 1.screenWidth,
              height: 240.height,
              color: Colors.transparent,
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.end,
                mainAxisAlignment: MainAxisAlignment.end,
                children: [
                  if (showVerticalVolumeSlider)
                    Container(
                      margin: EdgeInsets.only(left: 8.width, right: 8.width),
                      decoration: BoxDecoration(
                          color: LiveColors.designStandardG3.withAlpha(25),
                          borderRadius:
                              BorderRadius.only(topLeft: Radius.circular(6.radius), topRight: Radius.circular(6.radius))),
                      child: Column(
                        children: [
                          Container(
                            height: 160.0.height,
                            width: 24.0.width,
                            padding: EdgeInsets.symmetric(vertical: 4.0.height),
                            child: RotatedBox(
                              quarterTurns: 3,
                              child: ValueListenableBuilder(
                                  valueListenable: _volume,
                                  builder: (context, volume, _) {
                                    return Slider(
                                      value: volume,
                                      min: 0.0,
                                      max: 100,
                                      onChanged: (value) => _onVolumeChanged(value),
                                      activeColor: LiveColors.designStandardFlowkitWhite,
                                      inactiveColor: LiveColors.designSliderColorEmpty,
                                    );
                                  }),
                            ),
                          ),
                          ValueListenableBuilder(
                              valueListenable: _volume,
                              builder: (context, volume, _) {
                                return Text(
                                  '${_volume.value.toInt()}',
                                  style: const TextStyle(color: LiveColors.designStandardFlowkitWhite),
                                );
                              })
                        ],
                      ),
                    ),
                  Container(
                    decoration: BoxDecoration(
                        color: LiveColors.notStandardPureBlack.withAlpha(25),
                        borderRadius: BorderRadius.all(Radius.circular(12.radius))),
                    child: Row(
                      mainAxisAlignment: MainAxisAlignment.spaceBetween,
                      children: [
                        ValueListenableBuilder(
                            valueListenable: _isPlaying,
                            builder: (context, isPlaying, _) {
                              return IconButton(
                                icon: Icon(
                                  isPlaying ? Icons.pause : Icons.play_arrow,
                                  color: Colors.white,
                                  size: 32.radius,
                                ),
                                onPressed: () => _onPlayModeButtonClicked(),
                              );
                            }),
                        ValueListenableBuilder(
                            valueListenable: _volume,
                            builder: (context, volume, _) {
                              return IconButton(
                                icon: Icon(
                                  volume == 0
                                      ? Icons.volume_off
                                      : volume < 50
                                          ? Icons.volume_down
                                          : Icons.volume_up,
                                  color: Colors.white,
                                  size: 28.radius,
                                ),
                                onPressed: () {
                                  _showVerticalVolumeSlider.value = !_showVerticalVolumeSlider.value;
                                },
                              );
                            })
                      ],
                    ),
                  ),
                  SizedBox(height: 15.height)
                ],
              ),
            ),
          );
        });
  }
}

extension on _PlayerMenuWidgetState {
  void _onVolumeChanged(double newVolume) {
    _volume.value = newVolume;
    widget.liveStreamManager.setAudioPlayoutVolume(newVolume.toInt());
  }

  void _onPlayModeButtonClicked() {
    final toPause = _isPlaying.value;
    _isPlaying.value = !_isPlaying.value;
    toPause ? _pause() : _resume();
  }

  void _pause() {
    widget.liveStreamManager.pauseByAudience();
  }

  void _resume() {
    widget.liveStreamManager.resumeByAudience();
  }

  void _onLiveStatusChanged() {
    if (mounted && widget.liveStreamManager.roomState.liveStatus.value == LiveStatus.finished) {
      Navigator.pop(context);
    }
  }

  void _handleKickedOut() {
    if (mounted) {
      Navigator.pop(context);
    }
  }
}
