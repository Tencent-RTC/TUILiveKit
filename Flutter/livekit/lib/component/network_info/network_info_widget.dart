import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/component/network_info/manager/network_info_manager.dart';
import 'package:tencent_live_uikit/component/network_info/state/network_info_state.dart';
import 'package:tencent_live_uikit/tencent_live_uikit.dart';

class NetworkInfoWidget extends StatefulWidget {
  final bool isAudience;
  final NetworkInfoManager manager;

  const NetworkInfoWidget({super.key, required this.manager, required this.isAudience});

  @override
  State<NetworkInfoWidget> createState() => _NetworkInfoWidgetState();
}

class _NetworkInfoWidgetState extends State<NetworkInfoWidget> {

  @override
  void initState() {
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    return Container(
      padding: EdgeInsets.only(top: 16.height, left: 20.width, right: 20.width),
      constraints: BoxConstraints(maxHeight: 457.height),
      decoration: const BoxDecoration(
        color: LiveColors.designBgColorOperate,
        borderRadius: BorderRadius.only(
          topLeft: Radius.circular(20),
          topRight: Radius.circular(20),
        ),
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        mainAxisSize: MainAxisSize.min,
        children: [
          Center(
            child: Text(
              LiveKitLocalizations.of(context)!.common_live_info,
              style: TextStyle(
                  fontSize: 16,
                  fontWeight: FontWeight.w500,
                  color: LiveColors.designStandardFlowkitWhite.withAlpha(0xE6)),
            ),
          ),
          Visibility(visible: !widget.isAudience, child: const SizedBox(height: 16)),
          Visibility(
            visible: !widget.isAudience,
            child: ValueListenableBuilder(
                valueListenable: widget.manager.state.videoState,
                builder: (context, videoState, _) {
                  late String statusText;
                  late String iconName;
                  switch (videoState) {
                    case VideoState.close:
                      statusText = LiveKitLocalizations.of(context)!.common_close;
                      iconName = LiveImages.networkInfoVideoError;
                      break;
                    case VideoState.normal:
                      statusText = LiveKitLocalizations.of(context)!.common_normal;
                      iconName = LiveImages.networkInfoVideo;
                      break;
                    case VideoState.exception:
                      statusText = LiveKitLocalizations.of(context)!.common_exception;
                      iconName = LiveImages.networkInfoVideoError;
                      break;
                    default:
                      statusText = LiveKitLocalizations.of(context)!.common_exception;
                      iconName = LiveImages.networkInfoVideoError;
                      break;
                  }
                  return Row(
                    children: [
                      Image.asset(iconName,
                          width: 20.width, height: 20.width, package: Constants.pluginName),
                      SizedBox(width: 6.width),
                      Text('${LiveKitLocalizations.of(context)!.common_video_status}$statusText',
                          style: TextStyle(
                              fontSize: 14,
                              fontWeight: FontWeight.w500,
                              color: LiveColors.designStandardFlowkitWhite.withAlpha(0xE6))),
                    ],
                  );
                }),
          ),
          Visibility(
            visible: !widget.isAudience,
            child: ListenableBuilder(
                listenable: Listenable.merge([widget.manager.state.videoState, widget.manager.state.videoResolution]),
                builder: (context, _) {
                  final videoState = widget.manager.state.videoState.value;
                  late String detailText;
                  switch (videoState) {
                    case VideoState.close:
                      detailText = LiveKitLocalizations.of(context)!.common_video_capture_closed;
                      break;
                    case VideoState.normal:
                      detailText = LiveKitLocalizations.of(context)!.common_video_stream_smooth;
                      break;
                    case VideoState.exception:
                      detailText = LiveKitLocalizations.of(context)!.common_video_stream_freezing;
                      break;
                    default:
                      detailText = LiveKitLocalizations.of(context)!.common_video_stream_freezing;
                      break;
                  }
                  final videoResolution = widget.manager.state.videoResolution.value;
                  return Padding(
                      padding: EdgeInsets.only(left: 26.width, top: 4.height, bottom: 24.height),
                      child: Row(
                        mainAxisAlignment: MainAxisAlignment.spaceBetween,
                        children: [
                          Text(
                            detailText,
                            style: TextStyle(
                                fontSize: 12,
                                fontWeight: FontWeight.w400,
                                color: LiveColors.designStandardFlowkitWhite.withAlpha(0x8C)),
                          ),
                          Text(
                            '| ${LiveKitLocalizations.of(context)!.live_clarity}：${videoResolution}P',
                            style: TextStyle(
                                fontSize: 12,
                                fontWeight: FontWeight.w400,
                                color: LiveColors.designStandardFlowkitWhite.withAlpha(0x8C)),
                          ),
                        ],
                      ));
                }),
          ),
          Visibility(
            visible: !widget.isAudience,
            child: ValueListenableBuilder(
              valueListenable: widget.manager.state.audioState,
              builder: (context, audioState, _) {
                final audioState = widget.manager.state.audioState.value;
                late String statusText;
                switch (audioState) {
                  case AudioState.close:
                  case AudioState.mute:
                    statusText = LiveKitLocalizations.of(context)!.common_close;
                    break;
                  case AudioState.normal:
                    statusText = LiveKitLocalizations.of(context)!.common_normal;
                    break;
                  case AudioState.exception:
                    statusText = LiveKitLocalizations.of(context)!.common_exception;
                    break;
                  default:
                    statusText = LiveKitLocalizations.of(context)!.common_exception;
                    break;
                }
                return Row(
                  children: [
                    Image.asset(LiveImages.networkInfoMic,
                        width: 20.width, height: 20.width, package: Constants.pluginName),
                    SizedBox(width: 6.width),
                    Text('${LiveKitLocalizations.of(context)!.common_audio_status}$statusText',
                        style: TextStyle(
                            fontSize: 14,
                            fontWeight: FontWeight.w500,
                            color: LiveColors.designStandardFlowkitWhite.withAlpha(0xE6))),
                  ],
                );
              },
            ),
          ),
          Visibility(
            visible: !widget.isAudience,
            child: ValueListenableBuilder(
                valueListenable: widget.manager.state.audioQuality,
                builder: (context, audioQuality, _) {
                  final audioQuality = widget.manager.state.audioQuality.value;
                  late String qualityText;
                  switch (audioQuality) {
                    case TUIAudioQuality.audioProfileDefault:
                      qualityText = LiveKitLocalizations.of(context)!.common_audio_mode_default;
                      break;
                    case TUIAudioQuality.audioProfileSpeech:
                      qualityText = LiveKitLocalizations.of(context)!.common_audio_mode_speech;
                      break;
                    case TUIAudioQuality.audioProfileMusic:
                      qualityText = LiveKitLocalizations.of(context)!.common_audio_mode_music;
                      break;
                    default:
                      qualityText = LiveKitLocalizations.of(context)!.common_audio_mode_default;
                      break;
                  }
                  return Padding(
                    padding: EdgeInsets.only(left: 26.width, top: 4.height),
                    child: Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: [
                        Row(mainAxisAlignment: MainAxisAlignment.spaceBetween, children: [
                          Text('${LiveKitLocalizations.of(context)!.common_audio_tips_proper_volume} ',
                              style: TextStyle(
                                  fontSize: 12,
                                  fontWeight: FontWeight.w400,
                                  color: LiveColors.designStandardFlowkitWhite.withAlpha(0x8C))),
                          GestureDetector(
                            onTap: () => _closeMusicMode(),
                            child: Text('| $qualityText >',
                                style: TextStyle(
                                    fontSize: 12,
                                    fontWeight: FontWeight.w400,
                                    color: LiveColors.designStandardFlowkitWhite.withAlpha(0x8C))),
                          ),
                        ]),
                        SizedBox(height: 7.height),
                        ValueListenableBuilder(
                            valueListenable: widget.manager.state.volume,
                            builder: (context, volume, _) {
                              return Row(
                                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                                children: [
                                  SizedBox(
                                    width: 280.width,
                                    height: 14.height,
                                    child: Slider(
                                      value: volume.toDouble(),
                                      min: 0,
                                      max: 100,
                                      activeColor: LiveColors.designSliderColorFilled,
                                      inactiveColor: LiveColors.designSliderColorEmpty,
                                      thumbColor: LiveColors.designStandardFlowkitWhite,
                                      onChanged: (value) {
                                        widget.manager.handleAudioSliderChanged(value.toInt());
                                      },
                                    ),
                                  ),
                                  Text(
                                    '$volume',
                                    style: TextStyle(
                                        fontSize: 14,
                                        fontWeight: FontWeight.w400,
                                        color: LiveColors.designStandardFlowkitWhite.withAlpha(0x8C)),
                                  ),
                                ],
                              );
                            })
                      ],
                    ),
                  );
                }),
          ),
          SizedBox(height: 24.height),
          ValueListenableBuilder(
              valueListenable: widget.manager.state.deviceTemperature,
              builder: (context, deviceTemperature, _) {
                late String statusText;
                late String iconName;
                switch (deviceTemperature) {
                  case 0:
                    statusText = LiveKitLocalizations.of(context)!.common_normal;
                    iconName = LiveImages.networkInfoTemperature;
                    break;
                  case 1:
                    statusText = LiveKitLocalizations.of(context)!.common_device_temp_fair;
                    iconName = LiveImages.networkInfoTemperatureWarn;
                    break;
                  case 2:
                  case 3:
                    statusText = LiveKitLocalizations.of(context)!.common_device_temp_serious;
                    iconName = LiveImages.networkInfoTemperatureError;
                    break;
                  default:
                    statusText = LiveKitLocalizations.of(context)!.common_normal;
                    iconName = LiveImages.networkInfoTemperatureError;
                }
                return Row(
                  children: [
                    Image.asset(iconName, width: 20.width, height: 20.width, package: Constants.pluginName),
                    SizedBox(width: 6.width),
                    Text('${LiveKitLocalizations.of(context)!.common_device_temp}$statusText',
                        style: TextStyle(
                            fontSize: 14,
                            fontWeight: FontWeight.w500,
                            color: LiveColors.designStandardFlowkitWhite.withAlpha(0xE6))),
                  ],
                );
              }),
          Padding(
            padding: EdgeInsets.only(left: 26.width, top: 4.height, bottom: 24.height),
            child: Text(
              LiveKitLocalizations.of(context)!.common_audio_tips_regular_checks,
              style: TextStyle(
                  fontSize: 12,
                  fontWeight: FontWeight.w400,
                  color: LiveColors.designStandardFlowkitWhite.withAlpha(0x8C)),
            ),
          ),
          ValueListenableBuilder(
              valueListenable: widget.manager.state.networkQuality,
              builder: (context, networkQuality, _) {
                late String statusText;
                late String iconName;
                switch (networkQuality) {
                  case TUINetworkQuality.qualityExcellent:
                    statusText = LiveKitLocalizations.of(context)!.common_excellent;
                    iconName = LiveImages.networkInfoWifi;
                    break;
                  case TUINetworkQuality.qualityGood:
                    statusText = LiveKitLocalizations.of(context)!.common_good;
                    iconName = LiveImages.networkInfoWifi;
                    break;
                  case TUINetworkQuality.qualityPoor:
                    statusText = LiveKitLocalizations.of(context)!.common_poor;
                    iconName = LiveImages.networkInfoWifiPoor;
                    break;
                  case TUINetworkQuality.qualityBad:
                    statusText = LiveKitLocalizations.of(context)!.common_bad;
                    iconName = LiveImages.networkInfoWifiBad;
                    break;
                  case TUINetworkQuality.qualityVeryBad:
                    statusText = LiveKitLocalizations.of(context)!.common_verybad;
                    iconName = LiveImages.networkInfoWifiError;
                    break;
                  case TUINetworkQuality.qualityDown:
                    statusText = LiveKitLocalizations.of(context)!.common_down;
                    iconName = LiveImages.networkInfoWifiError;
                    break;
                  default:
                    statusText = LiveKitLocalizations.of(context)!.common_down;
                    iconName = LiveImages.networkInfoWifiError;
                    break;
                }
                return Row(
                  children: [
                    Image.asset(iconName, width: 20.width, height: 20.width, package: Constants.pluginName),
                    SizedBox(width: 6.width),
                    Text('${LiveKitLocalizations.of(context)!.common_wifi_or_mobile_network}$statusText',
                        style: TextStyle(
                            fontSize: 14,
                            fontWeight: FontWeight.w500,
                            color: LiveColors.designStandardFlowkitWhite.withAlpha(0xE6))),
                  ],
                );
              }),
          Padding(
            padding: EdgeInsets.only(left: 26.width, top: 4.height, bottom: 8.height),
            child: Text(
              LiveKitLocalizations.of(context)!.common_network_switch_tips,
              style: TextStyle(
                  fontSize: 12,
                  fontWeight: FontWeight.w400,
                  color: LiveColors.designStandardFlowkitWhite.withAlpha(0x8C)),
            ),
          ),
          // 网络指标
          Container(
            padding: EdgeInsets.symmetric(horizontal: 8.5.width, vertical: 8.height),
            decoration: BoxDecoration(
              color: LiveColors.designBgColorInput,
              borderRadius: BorderRadius.circular(10.radius),
            ),
            child: Row(
              mainAxisAlignment: MainAxisAlignment.spaceAround,
              children: [
                ListenableBuilder(
                    listenable: Listenable.merge([widget.manager.state.rtt, widget.manager.state.networkQuality]),
                    builder: (context, _) {
                      final rtt = widget.manager.state.rtt.value;
                      final networkQuality = widget.manager.state.networkQuality.value;
                      late Color textColor;
                      if (rtt >= 0 && rtt < 30) {
                        textColor = LiveColors.designTextColorSuccess;
                      } else if (rtt >= 30 && rtt < 100) {
                        textColor = LiveColors.designTextColorWarning;
                      } else {
                        textColor = LiveColors.notStandardRed;
                      }

                      if (networkQuality == TUINetworkQuality.qualityExcellent ||
                          networkQuality == TUINetworkQuality.qualityGood ||
                          networkQuality == TUINetworkQuality.qualityPoor) {
                        textColor = LiveColors.designTextColorSuccess;
                      }
                      if (networkQuality == TUINetworkQuality.qualityBad ||
                          networkQuality == TUINetworkQuality.qualityVeryBad ||
                          networkQuality == TUINetworkQuality.qualityDown) {
                        textColor = LiveColors.notStandardRed;
                      }

                      return _NetworkInfo(
                          label: LiveKitLocalizations.of(context)!.common_rtt, value: '${rtt}ms', color: textColor);
                    }),
                ListenableBuilder(
                    listenable: Listenable.merge([widget.manager.state.downLoss, widget.manager.state.networkQuality]),
                    builder: (context, _) {
                      final downLoss = widget.manager.state.downLoss.value;
                      final networkQuality = widget.manager.state.networkQuality.value;
                      late Color textColor;
                      if (downLoss >= 0 && downLoss < 5) {
                        textColor = LiveColors.designTextColorSuccess;
                      } else if (downLoss >= 5 && downLoss < 10) {
                        textColor = LiveColors.designTextColorWarning;
                      } else {
                        textColor = LiveColors.notStandardRed;
                      }

                      if (networkQuality == TUINetworkQuality.qualityExcellent ||
                          networkQuality == TUINetworkQuality.qualityGood ||
                          networkQuality == TUINetworkQuality.qualityPoor) {
                        textColor = LiveColors.designTextColorSuccess;
                      }
                      if (networkQuality == TUINetworkQuality.qualityBad ||
                          networkQuality == TUINetworkQuality.qualityVeryBad ||
                          networkQuality == TUINetworkQuality.qualityDown) {
                        textColor = LiveColors.notStandardRed;
                      }

                      return _NetworkInfo(
                          label: LiveKitLocalizations.of(context)!.common_down_loss,
                          value: '$downLoss%',
                          color: textColor);
                    }),
                ListenableBuilder(
                    listenable: Listenable.merge([widget.manager.state.upLoss, widget.manager.state.networkQuality]),
                    builder: (context, _) {
                      final upLoss = widget.manager.state.upLoss.value;
                      final networkQuality = widget.manager.state.networkQuality.value;
                      late Color textColor;
                      if (upLoss >= 0 && upLoss < 5) {
                        textColor = LiveColors.designTextColorSuccess;
                      } else if (upLoss >= 5 && upLoss < 10) {
                        textColor = LiveColors.designTextColorWarning;
                      } else {
                        textColor = LiveColors.notStandardRed;
                      }

                      if (networkQuality == TUINetworkQuality.qualityExcellent ||
                          networkQuality == TUINetworkQuality.qualityGood ||
                          networkQuality == TUINetworkQuality.qualityPoor) {
                        textColor = LiveColors.designTextColorSuccess;
                      }
                      if (networkQuality == TUINetworkQuality.qualityBad ||
                          networkQuality == TUINetworkQuality.qualityVeryBad ||
                          networkQuality == TUINetworkQuality.qualityDown) {
                        textColor = LiveColors.notStandardRed;
                      }

                      return _NetworkInfo(
                        label: LiveKitLocalizations.of(context)!.common_up_loss,
                        value: '$upLoss%',
                        color: textColor,
                      );
                    })
              ],
            ),
          ),
          SizedBox(height: 34.height)
        ],
      ),
    );
  }
}

class _NetworkInfo extends StatelessWidget {
  final String label;
  final String value;
  final Color color;

  const _NetworkInfo({required this.label, required this.value, this.color = Colors.white});

  @override
  Widget build(BuildContext context) {
    return Column(
      children: [
        Text(
          value,
          style: TextStyle(fontSize: 16, fontWeight: FontWeight.w500, color: color),
        ),
        const SizedBox(height: 4),
        Text(
          label,
          style: TextStyle(
              fontSize: 12, fontWeight: FontWeight.w400, color: LiveColors.designStandardFlowkitWhite.withAlpha(0x8C)),
        ),
      ],
    );
  }
}

extension on _NetworkInfoWidgetState {
  void _closeMusicMode() {
    const defaultModeNumber = 1;
    const musicModeNumber = 2;
    const speechModeNumber = 3;
    const cancelNumber = 4;
    final List<ActionSheetModel> menuData = List.empty(growable: true);

    const lineColor = LiveColors.designBgColorInput;
    final textColor = LiveColors.designStandardFlowkitWhite.withAlpha(0xE6);

    final defaultMode = ActionSheetModel(
        isCenter: true,
        text: LiveKitLocalizations.of(context)!.common_audio_mode_default,
        textStyle: TextStyle(color: textColor, fontSize: 16),
        lineColor: lineColor,
        bingData: defaultModeNumber);
    menuData.add(defaultMode);

    final musicMode = ActionSheetModel(
        isCenter: true,
        text: LiveKitLocalizations.of(context)!.common_audio_mode_music,
        textStyle: TextStyle(color: textColor, fontSize: 16),
        lineColor: lineColor,
        bingData: musicModeNumber);
    menuData.add(musicMode);

    final speechMode = ActionSheetModel(
        isCenter: true,
        text: LiveKitLocalizations.of(context)!.common_audio_mode_speech,
        textStyle: TextStyle(color: textColor, fontSize: 16),
        lineColor: lineColor,
        bingData: speechModeNumber);
    menuData.add(speechMode);

    final cancel = ActionSheetModel(
        isCenter: true,
        text: LiveKitLocalizations.of(context)!.common_cancel,
        textStyle: TextStyle(color: textColor, fontSize: 16),
        lineColor: lineColor,
        bingData: cancelNumber);
    menuData.add(cancel);

    ActionSheet.show(menuData, (model) {
      switch (model.bingData) {
        case defaultModeNumber:
          widget.manager.onAudioQualityChanged(TUIAudioQuality.audioProfileDefault);
          break;
        case musicModeNumber:
          widget.manager.onAudioQualityChanged(TUIAudioQuality.audioProfileMusic);
          break;
        case speechModeNumber:
          widget.manager.onAudioQualityChanged(TUIAudioQuality.audioProfileSpeech);
          break;
        default:
          break;
      }
    }, backgroundColor: LiveColors.designBgColorOperate);
  }
}
