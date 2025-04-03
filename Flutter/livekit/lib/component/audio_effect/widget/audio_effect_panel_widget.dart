import 'package:flutter/material.dart';

import '../../../common/index.dart';
import '../service/audio_effect_service.dart';
import '../store/audio_effect_state.dart';
import 'change_voice_widget.dart';
import 'reverb_widget.dart';

class AudioEffectPanelWidget extends BasicWidget {
  const AudioEffectPanelWidget({super.key, required super.liveController});

  @override
  BasicState getState() {
    return AudioEffectPanelWidgetState();
  }
}

class AudioEffectPanelWidgetState extends BasicState<AudioEffectPanelWidget> {
  late double _screenWidth;
  late AudioEffectService audioEffectService;

  @override
  void initState() {
    super.initState();
    _initData();
  }

  @override
  Widget build(BuildContext context) {
    _screenWidth = MediaQuery.of(context).size.width;
    return Container(
      width: _screenWidth,
      height: 718,
      decoration: const BoxDecoration(
        color: LiveColors.designStandardG2,
        borderRadius: BorderRadius.only(topLeft: Radius.circular(20), topRight: Radius.circular(20)),
      ),
      child: Column(children: [
        _initTitleWidget(),
        _initEarReturnWidget(),
        _initAudioSettingWidget(),
        _initChangeVoiceWidget(),
        _initReverbWidget(),
      ]),
    );
  }

  _initTitleWidget() {
    return SizedBox(
      height: 44,
      width: _screenWidth,
      child: Stack(
        children: [
          Positioned(
            left: 14,
            child: GestureDetector(
              onTap: () {
                Navigator.pop(context);
              },
              child: Container(
                width: 44,
                height: 44,
                padding: const EdgeInsets.all(10),
                child: Image.asset(
                  LiveImages.returnArrow,
                  package: Constants.pluginName,
                ),
              ),
            ),
          ),
          Center(
            child: Text(
              LiveKitLocalizations.of(Global.appContext())!.live_audio_effect,
              style: const TextStyle(color: LiveColors.designStandardG7, fontSize: 16),
            ),
          ),
        ],
      ),
    );
  }

  _initEarReturnWidget() {
    return Container(
      height: 112,
      margin: const EdgeInsets.only(left: 16, top: 20, right: 16),
      decoration: const BoxDecoration(
        color: LiveColors.notStandardBlue30Transparency,
        borderRadius: BorderRadius.all(Radius.circular(10)),
      ),
      padding: const EdgeInsets.only(left: 12, right: 12),
      child: Column(
        children: [
          SizedBox(
            height: 55,
            child: Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                crossAxisAlignment: CrossAxisAlignment.center,
                children: [
                  Text(
                    LiveKitLocalizations.of(Global.appContext())!.live_ear_return,
                    style: const TextStyle(color: LiveColors.notStandardWhite, fontSize: 16),
                  ),
                  ValueListenableBuilder(
                    valueListenable: audioEffectService.audioEffectState.enableVoiceEarMonitor,
                    builder: (BuildContext context, bool value, Widget? child) {
                      return Switch(
                        activeColor: LiveColors.notStandardWhite,
                        activeTrackColor: LiveColors.designStandardB1,
                        value: audioEffectService.audioEffectState.enableVoiceEarMonitor.value,
                        onChanged: (value) {
                          _enableEarReturn(value);
                        },
                      );
                    },
                  ),
                ]),
          ),
          Container(
            height: 0.5,
            color: LiveColors.designStandardG6,
          ),
          SizedBox(
            height: 55,
            child: Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                crossAxisAlignment: CrossAxisAlignment.center,
                children: [
                  Text(
                    LiveKitLocalizations.of(Global.appContext())!.live_ear_return_volume,
                    style: const TextStyle(color: LiveColors.notStandardWhite, fontSize: 16),
                  ),
                  ValueListenableBuilder(
                    valueListenable: audioEffectService.audioEffectState.earMonitorVolume,
                    builder: (BuildContext context, int value, Widget? child) {
                      return SizedBox(
                        width: 150,
                        child: Row(
                          children: [
                            Text(
                              audioEffectService.audioEffectState.earMonitorVolume.value.toString(),
                              style: const TextStyle(color: LiveColors.notStandardWhite, fontSize: 16),
                            ),
                            Expanded(
                                child: Slider(
                              min: 0,
                              max: 100,
                              value: audioEffectService.audioEffectState.earMonitorVolume.value.toDouble(),
                              activeColor: LiveColors.designStandardB1,
                              thumbColor: LiveColors.designStandardFlowkitWhite,
                              onChanged: (double value) {
                                _setEarReturnVolume(value);
                              },
                            )),
                          ],
                        ),
                      );
                    },
                  ),
                ]),
          ),
        ],
      ),
    );
  }

  _initAudioSettingWidget() {
    return Container(
      width: _screenWidth,
      margin: const EdgeInsets.only(left: 16, top: 20, right: 16),
      child: Column(
        mainAxisAlignment: MainAxisAlignment.start,
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            LiveKitLocalizations.of(Global.appContext())!.live_audio_settings,
            style: const TextStyle(color: LiveColors.designStandardG7, fontSize: 14),
          ),
          Container(
            height: 112,
            margin: const EdgeInsets.only(
              top: 10,
            ),
            decoration: const BoxDecoration(
              color: LiveColors.notStandardBlue30Transparency,
              borderRadius: BorderRadius.all(Radius.circular(10)),
            ),
            padding: const EdgeInsets.only(left: 12, right: 12),
            child: Column(
              children: [
                SizedBox(
                  height: 55,
                  child: Row(
                      mainAxisAlignment: MainAxisAlignment.spaceBetween,
                      crossAxisAlignment: CrossAxisAlignment.center,
                      children: [
                        Text(
                          LiveKitLocalizations.of(Global.appContext())!.live_music_volume,
                          style: const TextStyle(color: LiveColors.notStandardWhite, fontSize: 16),
                        ),
                        ValueListenableBuilder(
                          valueListenable: audioEffectService.audioEffectState.musicVolume,
                          builder: (BuildContext context, int value, Widget? child) {
                            return SizedBox(
                              width: 150,
                              child: Row(
                                children: [
                                  Text(
                                    audioEffectService.audioEffectState.musicVolume.value.toString(),
                                    style: const TextStyle(color: LiveColors.notStandardWhite, fontSize: 16),
                                  ),
                                  Expanded(
                                      child: Slider(
                                    min: 0,
                                    max: 100,
                                    value: audioEffectService.audioEffectState.musicVolume.value.toDouble(),
                                    activeColor: LiveColors.designStandardB1,
                                    thumbColor: LiveColors.designStandardFlowkitWhite,
                                    onChanged: (double value) {
                                      _setMusicVolume(value);
                                    },
                                  )),
                                ],
                              ),
                            );
                          },
                        ),
                      ]),
                ),
                Container(
                  height: 0.5,
                  color: LiveColors.designStandardG6,
                ),
                SizedBox(
                  height: 55,
                  child: Row(
                      mainAxisAlignment: MainAxisAlignment.spaceBetween,
                      crossAxisAlignment: CrossAxisAlignment.center,
                      children: [
                        Text(
                          LiveKitLocalizations.of(Global.appContext())!.live_people_volume,
                          style: const TextStyle(color: LiveColors.notStandardWhite, fontSize: 16),
                        ),
                        ValueListenableBuilder(
                          valueListenable: audioEffectService.audioEffectState.voiceVolume,
                          builder: (BuildContext context, int value, Widget? child) {
                            return SizedBox(
                              width: 150,
                              child: Row(
                                children: [
                                  Text(
                                    audioEffectService.audioEffectState.voiceVolume.value.toString(),
                                    style: const TextStyle(color: LiveColors.notStandardWhite, fontSize: 16),
                                  ),
                                  Expanded(
                                      child: Slider(
                                    min: 0,
                                    max: 100,
                                    value: audioEffectService.audioEffectState.voiceVolume.value.toDouble(),
                                    activeColor: LiveColors.designStandardB1,
                                    thumbColor: LiveColors.designStandardFlowkitWhite,
                                    onChanged: (double value) {
                                      _setVoiceVolume(value);
                                    },
                                  )),
                                ],
                              ),
                            );
                          },
                        ),
                      ]),
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }

  _initChangeVoiceWidget() {
    return Container(
      width: _screenWidth,
      margin: const EdgeInsets.only(left: 16, top: 20, right: 16),
      child: Column(
        mainAxisAlignment: MainAxisAlignment.start,
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            LiveKitLocalizations.of(Global.appContext())!.live_change_voice,
            style: const TextStyle(color: LiveColors.designStandardG7, fontSize: 14),
          ),
          Container(
            height: 80,
            width: _screenWidth,
            margin: const EdgeInsets.only(top: 10),
            child: ChangeVoiceWidget(liveController: liveController, audioEffectService: audioEffectService),
          ),
        ],
      ),
    );
  }

  _initReverbWidget() {
    return Container(
      width: _screenWidth,
      margin: const EdgeInsets.only(left: 16, top: 20, right: 16),
      child: Column(
        mainAxisAlignment: MainAxisAlignment.start,
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            LiveKitLocalizations.of(Global.appContext())!.live_reverb,
            style: const TextStyle(color: LiveColors.designStandardG7, fontSize: 14),
          ),
          Container(
            height: 80,
            width: _screenWidth,
            margin: const EdgeInsets.only(top: 10),
            child: ReverbWidget(liveController: liveController, audioEffectService: audioEffectService),
          ),
        ],
      ),
    );
  }
}

extension AudioEffectPanelWidgetStateLogicExtension on AudioEffectPanelWidgetState {
  void _initData() {
    AudioEffectState audioEffectState = AudioEffectStateFactory.getState(liveController.getRoomSate().roomId);
    audioEffectService = AudioEffectService(liveController: liveController, audioEffectState: audioEffectState);
  }

  _enableEarReturn(bool enable) {
    audioEffectService.enableVoiceEarMonitor(enable);
  }

  _setEarReturnVolume(double volume) {
    audioEffectService.setVoiceEarMonitorVolume(volume.toInt());
  }

  _setMusicVolume(double volume) {
    audioEffectService.setMusicVolume(volume.toInt());
  }

  _setVoiceVolume(double volume) {
    audioEffectService.setVoiceVolume(volume.toInt());
  }
}
