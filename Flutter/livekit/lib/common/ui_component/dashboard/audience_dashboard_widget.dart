import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';

class AudienceDashboardWidget extends BasicWidget {
  const AudienceDashboardWidget({super.key, required super.liveController});

  @override
  AudienceDashboardWidgetState getState() {
    return AudienceDashboardWidgetState();
  }
}

class AudienceDashboardWidgetState extends BasicState<AudienceDashboardWidget> {
  @override
  Widget build(BuildContext context) {
    return Container(
      color: LivekitColors.livekitDesignStandardG2,
      child: Stack(
        alignment: Alignment.topCenter,
        children: [
          _initBackWidget(),
          _initTitleWidget(),
          _initAnchorAvatarWidget(),
          _initAnchorNameWidget(),
        ],
      ),
    );
  }

  _initBackWidget() {
    return Positioned(
      right: 16,
      top: 58,
      width: 24,
      height: 24,
      child: GestureDetector(
        onTap: () {
          _closeWidget();
        },
        child: Image.asset(
          LivekitImages.livekitAudienceClose,
          package: Constants.pluginName,
        ),
      ),
    );
  }

  _initTitleWidget() {
    return Positioned(
      top: 120,
      child: GestureDetector(
        onTap: () {
          _closeWidget();
        },
        child: Text(
          LiveKitLocalizations.of(Global.appContext())!.livekit_live_has_stop,
          style: const TextStyle(color: LivekitColors.livekitDesignStandardFlowkitWhite, fontSize: 20),
        ),
      ),
    );
  }

  _initAnchorAvatarWidget() {
    return Positioned(
      top: 190,
      child: Container(
          margin: const EdgeInsets.only(left: 4, right: 8),
          width: 80,
          height: 80,
          child: ClipOval(
            child: Image.network(
              liveController.getRoomSate().ownerInfo.avatarUrl.value ?? "",
              fit: BoxFit.cover,
              errorBuilder: (context, error, stackTrace) {
                return Image.asset(
                  LivekitImages.livekitDefaultAvatar,
                  package: Constants.pluginName,
                );
              },
            ),
          )),
    );
  }

  _initAnchorNameWidget() {
    return Positioned(
      top: 275,
      child: Text(
        liveController.getRoomSate().ownerInfo.name.value ?? "",
        overflow: TextOverflow.ellipsis,
        style: const TextStyle(
            fontSize: 14, fontStyle: FontStyle.normal, color: LivekitColors.livekitDesignStandardFlowkitWhite),
      ),
    );
  }
}

extension AudienceDashboardWidgetStateLogicExtension on AudienceDashboardWidgetState {
  void _closeWidget() {
    Navigator.of(context).pop();
  }
}
