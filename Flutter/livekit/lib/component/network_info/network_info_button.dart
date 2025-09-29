import 'dart:async';

import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/component/network_info/index.dart';
import 'package:tencent_live_uikit/component/network_info/manager/network_info_manager.dart';
import 'package:tencent_live_uikit/tencent_live_uikit.dart';

class NetworkInfoButton extends StatefulWidget {
  final NetworkInfoManager manager;
  final int createTime;
  final bool isAudience;

  const NetworkInfoButton({super.key, required this.manager, required this.createTime, required this.isAudience});

  @override
  State<NetworkInfoButton> createState() => _NetworkInfoButtonState();
}

class _NetworkInfoButtonState extends State<NetworkInfoButton> {
  Timer? _durationTimer;
  final ValueNotifier<String> _formatDuration = ValueNotifier('00:00:00');

  @override
  void initState() {
    super.initState();
    _startDurationTimer();
  }

  @override
  void dispose() {
    _stopDurationTimer();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return GestureDetector(
      onTap: () {
        _popupWidget(NetworkInfoWidget(manager: widget.manager, isAudience: widget.isAudience));
      },
      child: Container(
          constraints: BoxConstraints(maxHeight: 20.height, maxWidth: 86.width),
          decoration: BoxDecoration(
              color: LiveColors.notStandardPureBlack.withAlpha(0x60), borderRadius: BorderRadius.circular(11.height)),
          padding: EdgeInsets.only(left: 4.width, top: 2.height, bottom: 2.height, right: 8.width),
          child: Row(mainAxisSize: MainAxisSize.min, children: [
            ValueListenableBuilder(
                valueListenable: widget.manager.state.networkQuality,
                builder: (context, networkQuality, _) {
                  final imagePath = _getNetworkWifiImagePathByNetworkQuality(networkQuality);
                  return Image.asset(imagePath, package: Constants.pluginName, width: 14.radius, height: 14.radius);
                }),
            SizedBox(width: 8.width),
            ValueListenableBuilder(
                valueListenable: _formatDuration,
                builder: (context, formatDuration, _) {
                  return Text(formatDuration,
                      style: TextStyle(
                          fontWeight: FontWeight.w400,
                          fontSize: 10,
                          color: LiveColors.designStandardFlowkitWhite.withAlpha(0xE6)));
                })
          ])),
    );
  }
}

extension on _NetworkInfoButtonState {
  String _getNetworkWifiImagePathByNetworkQuality(TUINetworkQuality quality) {
    switch (quality) {
      case TUINetworkQuality.qualityExcellent:
        return LiveImages.networkInfoWifi;
      case TUINetworkQuality.qualityGood:
        return LiveImages.networkInfoWifi;
      case TUINetworkQuality.qualityPoor:
        return LiveImages.networkInfoWifiPoor;
      case TUINetworkQuality.qualityBad:
        return LiveImages.networkInfoWifiBad;
      case TUINetworkQuality.qualityVeryBad:
        return LiveImages.networkInfoWifiError;
      case TUINetworkQuality.qualityDown:
        return LiveImages.networkInfoWifiError;
      default:
        return LiveImages.networkInfoWifiError;
    }
  }
}

extension on _NetworkInfoButtonState {
  void _popupWidget(Widget widget, {Color? barrierColor}) {
    showModalBottomSheet(
      barrierColor: barrierColor,
      isScrollControlled: true,
      context: Global.appContext(),
      backgroundColor: LiveColors.designStandardTransparent,
      builder: (context) => Container(
        decoration: BoxDecoration(
          borderRadius: BorderRadius.only(
            topLeft: Radius.circular(20.width),
            topRight: Radius.circular(20.width),
          ),
          color: LiveColors.designStandardTransparent,
        ),
        child: widget,
      ),
    );
  }
}

extension on _NetworkInfoButtonState {
  void _startDurationTimer() {
    _stopDurationTimer();

    _durationTimer = Timer.periodic(const Duration(milliseconds: 100), (_) {
      _updateFormatTime();
    });
  }

  void _stopDurationTimer() {
    _durationTimer?.cancel();
    _durationTimer = null;
  }

  void _updateFormatTime() {
    final currentDuration = (DateTime.now().millisecondsSinceEpoch / 1000.0 - widget.createTime / 1000.0).toInt();
    final hours = (currentDuration ~/ 3600).toString().padLeft(2, '0');
    final minutes = ((currentDuration % 3600) ~/ 60).toString().padLeft(2, '0');
    final seconds = (currentDuration % 60).toString().padLeft(2, '0');
    _formatDuration.value = '$hours:$minutes:$seconds';
  }
}
