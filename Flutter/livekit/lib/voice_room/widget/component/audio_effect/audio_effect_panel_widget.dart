import 'package:flutter/material.dart';

import '../../../../common/constants/index.dart';
import '../../../../common/language/index.dart';
import '../../../../common/resources/index.dart';
import '../../../../common/widget/index.dart';
import '../../../../common/screen/index.dart';
import 'manager/audio_effect_manager.dart';
import 'state/audio_effect_state.dart';
import 'state/audio_effect_state_factory.dart';
import 'widget/change_voice_widget.dart';
import 'widget/reverb_widget.dart';

class AudioEffectPanelWidget extends StatefulWidget {
  final String roomId;

  const AudioEffectPanelWidget({super.key, required this.roomId});

  @override
  State<AudioEffectPanelWidget> createState() => _AudioEffectPanelWidgetState();
}

class _AudioEffectPanelWidgetState extends State<AudioEffectPanelWidget> {
  late double _screenWidth;
  late AudioEffectManager manager;

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
      height: context.adapter.getHeight(663),
      decoration: BoxDecoration(
        color: LiveColors.designStandardG2,
        borderRadius: BorderRadius.only(
            topLeft: Radius.circular(context.adapter.getWidth(20)),
            topRight: Radius.circular(context.adapter.getWidth(20))),
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
      height: context.adapter.getHeight(44),
      width: _screenWidth,
      child: Stack(
        children: [
          Positioned(
            left: context.adapter.getWidth(14),
            child: GestureDetector(
              onTap: () {
                Navigator.pop(context);
              },
              child: Container(
                width: context.adapter.getWidth(44),
                height: context.adapter.getWidth(44),
                padding: EdgeInsets.all(context.adapter.getWidth(10)),
                child: Image.asset(
                  LiveImages.returnArrow,
                  package: Constants.pluginName,
                ),
              ),
            ),
          ),
          Center(
            child: Text(
              LiveKitLocalizations.of(Global.appContext())!
                  .live_audio_effect,
              style: const TextStyle(
                  color: LiveColors.designStandardG7, fontSize: 16),
            ),
          ),
        ],
      ),
    );
  }

  _initEarReturnWidget() {
    return Container(
      height: context.adapter.getHeight(112),
      margin: EdgeInsets.only(
          left: context.adapter.getWidth(16),
          top: context.adapter.getHeight(20),
          right: context.adapter.getWidth(16)),
      decoration: BoxDecoration(
        color: LiveColors.notStandardBlue30Transparency,
        borderRadius:
            BorderRadius.all(Radius.circular(context.adapter.getWidth(10))),
      ),
      padding: EdgeInsets.symmetric(horizontal: context.adapter.getWidth(12)),
      child: Column(
        children: [
          SizedBox(
            height: context.adapter.getHeight(55),
            child: Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                crossAxisAlignment: CrossAxisAlignment.center,
                children: [
                  Text(
                    LiveKitLocalizations.of(Global.appContext())!
                        .live_ear_return,
                    style: const TextStyle(
                        color: LiveColors.notStandardWhite, fontSize: 16),
                  ),
                  ValueListenableBuilder(
                    valueListenable: manager.state.enableVoiceEarMonitor,
                    builder: (context, enableVoiceEarMonitor, child) {
                      return Switch(
                        activeColor: LiveColors.notStandardWhite,
                        activeTrackColor: LiveColors.designStandardB1,
                        value: enableVoiceEarMonitor,
                        onChanged: (value) {
                          _enableEarReturn(value);
                        },
                      );
                    },
                  ),
                ]),
          ),
          Container(
            height: context.adapter.getHeight(0.5),
            color: LiveColors.designStandardG6,
          ),
          SizedBox(
            height: context.adapter.getHeight(55),
            child: Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                crossAxisAlignment: CrossAxisAlignment.center,
                children: [
                  Text(
                    LiveKitLocalizations.of(Global.appContext())!
                        .live_ear_return_volume,
                    style: const TextStyle(
                        color: LiveColors.notStandardWhite, fontSize: 16),
                  ),
                  ValueListenableBuilder(
                    valueListenable: manager.state.earMonitorVolume,
                    builder: (context, earMonitorVolume, child) {
                      return SizedBox(
                        width: context.adapter.getWidth(150),
                        child: Row(
                          children: [
                            Text(
                              earMonitorVolume.toString(),
                              style: const TextStyle(
                                  color: LiveColors.notStandardWhite,
                                  fontSize: 16),
                            ),
                            Expanded(
                                child: Slider(
                              min: 0,
                              max: 100,
                              value: earMonitorVolume.toDouble(),
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
      margin: EdgeInsets.only(
          left: context.adapter.getWidth(16),
          top: context.adapter.getHeight(20),
          right: context.adapter.getWidth(16)),
      child: Column(
        mainAxisAlignment: MainAxisAlignment.start,
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            LiveKitLocalizations.of(Global.appContext())!
                .live_audio_settings,
            style: const TextStyle(
                color: LiveColors.designStandardG7, fontSize: 14),
          ),
          Container(
            height: context.adapter.getWidth(57),
            margin: EdgeInsets.only(
              top: context.adapter.getHeight(10),
            ),
            decoration: BoxDecoration(
              color: LiveColors.notStandardBlue30Transparency,
              borderRadius: BorderRadius.all(
                  Radius.circular(context.adapter.getWidth(10))),
            ),
            padding:
                EdgeInsets.symmetric(horizontal: context.adapter.getWidth(12)),
            child: Column(
              children: [
                SizedBox(
                  height: 55,
                  child: Row(
                      mainAxisAlignment: MainAxisAlignment.spaceBetween,
                      crossAxisAlignment: CrossAxisAlignment.center,
                      children: [
                        Text(
                          LiveKitLocalizations.of(Global.appContext())!
                              .live_people_volume,
                          style: const TextStyle(
                              color: LiveColors.notStandardWhite, fontSize: 16),
                        ),
                        ValueListenableBuilder(
                          valueListenable: manager.state.voiceVolume,
                          builder: (context, voiceVolume, child) {
                            return SizedBox(
                              width: context.adapter.getWidth(150),
                              child: Row(
                                children: [
                                  Text(
                                    voiceVolume.toString(),
                                    style: const TextStyle(
                                        color: LiveColors.notStandardWhite,
                                        fontSize: 16),
                                  ),
                                  Expanded(
                                      child: Slider(
                                    min: 0,
                                    max: 100,
                                    value: voiceVolume.toDouble(),
                                    activeColor: LiveColors.designStandardB1,
                                    thumbColor:
                                        LiveColors.designStandardFlowkitWhite,
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
      margin: EdgeInsets.only(
          left: context.adapter.getWidth(16),
          top: context.adapter.getHeight(20),
          right: context.adapter.getWidth(16)),
      child: Column(
        mainAxisAlignment: MainAxisAlignment.start,
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            LiveKitLocalizations.of(Global.appContext())!.live_change_voice,
            style: const TextStyle(
                color: LiveColors.designStandardG7, fontSize: 14),
          ),
          Container(
            width: _screenWidth,
            margin: EdgeInsets.only(top: context.adapter.getHeight(10)),
            child: ChangeVoiceWidget(manager: manager),
          ),
        ],
      ),
    );
  }

  _initReverbWidget() {
    return Container(
      width: _screenWidth,
      margin: EdgeInsets.only(
          left: context.adapter.getWidth(16),
          top: context.adapter.getHeight(20),
          right: context.adapter.getWidth(16)),
      child: Column(
        mainAxisAlignment: MainAxisAlignment.start,
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            LiveKitLocalizations.of(Global.appContext())!.live_reverb,
            style: const TextStyle(
                color: LiveColors.designStandardG7, fontSize: 14),
          ),
          Container(
            width: _screenWidth,
            margin: EdgeInsets.only(top: context.adapter.getHeight(10)),
            child: ReverbWidget(manager: manager),
          ),
        ],
      ),
    );
  }
}

extension on _AudioEffectPanelWidgetState {
  void _initData() {
    AudioEffectState state = AudioEffectStateFactory.getState(widget.roomId);
    manager = AudioEffectManager(roomId: widget.roomId, state: state);
  }

  void _enableEarReturn(bool enable) {
    manager.enableVoiceEarMonitor(enable);
  }

  void _setEarReturnVolume(double volume) {
    manager.setVoiceEarMonitorVolume(volume.toInt());
  }

  void _setVoiceVolume(double volume) {
    manager.setVoiceVolume(volume.toInt());
  }
}
