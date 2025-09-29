import 'dart:io';

import 'package:flutter/material.dart';
import 'package:flutter/gestures.dart';

import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/component/network_info/manager/network_info_manager.dart';

class NetworkStatusToastWidget extends StatefulWidget {
  final NetworkInfoManager manager;

  const NetworkStatusToastWidget({super.key, required this.manager});

  @override
  State<NetworkStatusToastWidget> createState() => _NetworkStatusToastWidgetState();
}

class _NetworkStatusToastWidgetState extends State<NetworkStatusToastWidget> {
  @override
  Widget build(BuildContext context) {
    return Container(
      constraints: BoxConstraints(maxHeight: 40.height, maxWidth: 302.width),
      padding: EdgeInsets.symmetric(horizontal: 16.width, vertical: 9.height),
      decoration: BoxDecoration(
        color: LiveColors.notStandardBlack.withAlpha(0x8C),
        borderRadius: BorderRadius.circular(6.radius),
      ),
      child: Row(
        mainAxisSize: MainAxisSize.min,
        children: [
          Image.asset(LiveImages.networkInfoTips, package: Constants.pluginName, width: 16.radius, height: 16.radius),
          SizedBox(width: 4.width),
          Expanded(
            child: Text.rich(
              TextSpan(
                children: [
                  TextSpan(
                      text: LiveKitLocalizations.of(context)!.common_network_bad_tips,
                      style: TextStyle(
                          fontSize: 14,
                          fontWeight: FontWeight.w500,
                          color: LiveColors.designStandardFlowkitWhite.withAlpha(0xE6))),
                  TextSpan(
                      text: LiveKitLocalizations.of(context)!.common_switch_network,
                      style: const TextStyle(
                          fontSize: 14, fontWeight: FontWeight.w500, color: LiveColors.notStandardBlueColor),
                      recognizer: TapGestureRecognizer()..onTap = () => _openAppWifiSettings()),
                ],
              ),
            ),
          ),
          SizedBox(width: 24.width),
          GestureDetector(
              onTap: () => widget.manager.onNetworkInfoStatusToastWidgetClosed(),
              child: SizedBox(
                  width: 16.radius,
                  height: 16.radius,
                  child: Image.asset(LiveImages.audienceClose, package: Constants.pluginName))),
        ],
      ),
    );
  }
}

extension on _NetworkStatusToastWidgetState {
  void _openAppWifiSettings() {
    if (Platform.isAndroid) {
      TUILiveKitPlatform.instance.openWifiSettings();
      return;
    }

    if (Platform.isIOS) {
      TUILiveKitPlatform.instance.openAppSettings();
      return;
    }
  }
}
