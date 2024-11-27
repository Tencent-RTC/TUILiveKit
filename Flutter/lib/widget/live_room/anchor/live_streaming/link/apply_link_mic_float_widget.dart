import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/state/index.dart';
import 'package:tencent_live_uikit/widget/live_room/anchor/live_streaming/link/anchor_link_mic_manage_panel_widget.dart';

class ApplyLinkMicFloatWidget extends BasicWidget {
  const ApplyLinkMicFloatWidget({super.key, required super.liveController});

  @override
  ApplyLinkMicFloatWidgetState getState() {
    return ApplyLinkMicFloatWidgetState();
  }
}

class ApplyLinkMicFloatWidgetState extends BasicState<ApplyLinkMicFloatWidget> {
  @override
  Widget build(BuildContext context) {
    return ValueListenableBuilder(
      valueListenable: liveController.getSeatState().seatApplicationList,
      builder: (BuildContext context, List<SeatApplication> value, Widget? child) {
        return Visibility(
          visible: liveController.getSeatState().seatApplicationList.value.isNotEmpty,
          child: GestureDetector(
            onTap: () {
              _showLinkMicManagePanelWidget();
            },
            child: Container(
              width: 114,
              height: 86,
              decoration: BoxDecoration(
                color: LivekitColors.livekitDesignStandardG2,
                border: Border.all(color: LivekitColors.livekitNotStandardWhite20Transparency, width: 2),
                borderRadius: const BorderRadius.all(Radius.circular(18)),
              ),
              child: Column(
                mainAxisAlignment: MainAxisAlignment.start,
                crossAxisAlignment: CrossAxisAlignment.center,
                children: [
                  _initLinkMicAudienceAvatarWidget(),
                  _initLinkMicAudienceCountWidget(),
                ],
              ),
            ),
          ),
        );
      },
    );
  }

  _initLinkMicAudienceAvatarWidget() {
    return Container(
      margin: const EdgeInsets.only(top: 18),
      height: 36,
      width: 75,
      child: Stack(
        children: [
          Positioned(
            right: 0,
            child: Visibility(
              visible: liveController.getSeatState().seatApplicationList.value.length > 2,
              child: Container(
                width: 36,
                height: 36,
                decoration: BoxDecoration(
                  color: LivekitColors.livekitDesignStandardG2,
                  borderRadius: BorderRadius.circular(18),
                ),
                padding: const EdgeInsets.all(2),
                child: Image.asset(
                  LivekitImages.livekitEllipsis,
                  package: Constants.pluginName,
                ),
              ),
            ),
          ),
          Positioned(
            right: 18,
            child: Visibility(
              visible: liveController.getSeatState().seatApplicationList.value.isNotEmpty,
              child: Container(
                width: 36,
                height: 36,
                decoration: BoxDecoration(
                  color: LivekitColors.livekitDesignStandardG2,
                  borderRadius: BorderRadius.circular(18),
                ),
                padding: const EdgeInsets.all(2),
                child: ClipOval(
                  child: Image.network(
                    liveController.getSeatState().seatApplicationList.value.isNotEmpty
                        ? liveController.getSeatState().seatApplicationList.value[0].avatarUrl ?? ""
                        : "",
                    fit: BoxFit.cover,
                    errorBuilder: (context, error, stackTrace) {
                      return Image.asset(
                        LivekitImages.livekitDefaultAvatar,
                        package: Constants.pluginName,
                      );
                    },
                  ),
                ),
              ),
            ),
          ),
          Positioned(
            right: 36,
            child: Visibility(
              visible: liveController.getSeatState().seatApplicationList.value.length > 1,
              child: Container(
                width: 36,
                height: 36,
                decoration: BoxDecoration(
                  color: LivekitColors.livekitDesignStandardG2,
                  borderRadius: BorderRadius.circular(18),
                ),
                padding: const EdgeInsets.all(2.0),
                child: ClipOval(
                  child: Image.network(
                    liveController.getSeatState().seatApplicationList.value.length > 1
                        ? liveController.getSeatState().seatApplicationList.value[1].avatarUrl ?? ""
                        : "",
                    fit: BoxFit.cover,
                    errorBuilder: (context, error, stackTrace) {
                      return Image.asset(
                        LivekitImages.livekitDefaultAvatar,
                        package: Constants.pluginName,
                      );
                    },
                  ),
                ),
              ),
            ),
          ),
        ],
      ),
    );
  }

  _initLinkMicAudienceCountWidget() {
    return Container(
      margin: const EdgeInsets.only(top: 3),
      child: Text(
        "${LiveKitLocalizations.of(Global.appContext())!.livekit_link_mic_down_title_popup}"
        "(${liveController.getSeatState().seatApplicationList.value.length})",
        style: const TextStyle(color: LivekitColors.livekitDesignStandardG5, fontSize: 12),
      ),
    );
  }
}

extension ApplyLinkMicFloatWidgetStateLogicExtension on ApplyLinkMicFloatWidgetState {
  void _showLinkMicManagePanelWidget() {
    showWidget(AnchorLinkMicManagePanelWidget(liveController: liveController));
  }
}
