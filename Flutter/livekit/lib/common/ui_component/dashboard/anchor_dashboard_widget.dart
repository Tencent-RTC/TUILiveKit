import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';

class AnchorDashboardWidget extends BasicWidget {
  const AnchorDashboardWidget({super.key, required super.liveController});

  @override
  AnchorDashboardWidgetState getState() {
    return AnchorDashboardWidgetState();
  }
}

class AnchorDashboardWidgetState extends BasicState<AnchorDashboardWidget> {
  @override
  Widget build(BuildContext context) {
    return Container(
      color: LivekitColors.livekitDesignStandardG2,
      child: Stack(
        alignment: Alignment.topCenter,
        children: [
          _initBackWidget(),
          _initTitleWidget(),
          _initDataWidget(),
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
          LivekitImages.livekitClose,
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

  _initDataWidget() {
    return Positioned(
      top: 200,
      child: Container(
        width: screenWidth - 30,
        height: 165,
        decoration: const BoxDecoration(
          color: LivekitColors.livekitNotStandardBlue30Transparency,
          borderRadius: BorderRadius.all(Radius.circular(20)),
        ),
        child: Column(
          mainAxisAlignment: MainAxisAlignment.start,
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Container(
              margin: const EdgeInsets.only(top: 5, left: 15),
              child: Text(
                LiveKitLocalizations.of(Global.appContext())!.livekit_common_this_live_data,
                style: const TextStyle(color: LivekitColors.livekitDesignStandardFlowkitWhite, fontSize: 14),
              ),
            ),
            SizedBox(
              height: 70,
              child: Row(
                mainAxisAlignment: MainAxisAlignment.spaceAround,
                crossAxisAlignment: CrossAxisAlignment.center,
                children: [
                  _buildDurationWidget(),
                  _buildViewersWidget(),
                  _buildMessageWidget(),
                ],
              ),
            ),
            SizedBox(
              height: 70,
              child: Row(
                mainAxisAlignment: MainAxisAlignment.spaceAround,
                crossAxisAlignment: CrossAxisAlignment.center,
                children: [
                  _buildGiftIncomeWidget(),
                  _buildGiftGiversWidget(),
                  _buildLikesWidget(),
                ],
              ),
            ),
          ],
        ),
      ),
    );
  }

  _buildDurationWidget() {
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      crossAxisAlignment: CrossAxisAlignment.center,
      children: [
        Text(
          "${(DateTime.now().millisecondsSinceEpoch - liveController.getRoomSate().createTime) ~/ 1000}",
          style: const TextStyle(color: LivekitColors.livekitDesignStandardFlowkitWhite, fontSize: 12),
        ),
        2.verticalSpace,
        Text(
          LiveKitLocalizations.of(Global.appContext())!.livekit_common_live_duration,
          style: const TextStyle(color: LivekitColors.livekitDesignStandardG5, fontSize: 12),
        ),
      ],
    );
  }

  _buildViewersWidget() {
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      crossAxisAlignment: CrossAxisAlignment.center,
      children: [
        Text(
          "${liveController.getRoomSate().liveExtraInfo.maxAudienceCount}",
          style: const TextStyle(color: LivekitColors.livekitDesignStandardFlowkitWhite, fontSize: 12),
        ),
        2.verticalSpace,
        Text(
          LiveKitLocalizations.of(Global.appContext())!.livekit_common_live_people_number,
          style: const TextStyle(color: LivekitColors.livekitDesignStandardG5, fontSize: 12),
        ),
      ],
    );
  }

  _buildMessageWidget() {
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      crossAxisAlignment: CrossAxisAlignment.center,
      children: [
        Text(
          "${liveController.getRoomSate().liveExtraInfo.messageCount}",
          style: const TextStyle(color: LivekitColors.livekitDesignStandardFlowkitWhite, fontSize: 12),
        ),
        2.verticalSpace,
        Text(
          LiveKitLocalizations.of(Global.appContext())!.livekit_common_message_count,
          style: const TextStyle(color: LivekitColors.livekitDesignStandardG5, fontSize: 12),
        ),
      ],
    );
  }

  _buildGiftIncomeWidget() {
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      crossAxisAlignment: CrossAxisAlignment.center,
      children: [
        Text(
          "${liveController.getRoomSate().liveExtraInfo.giftIncome}",
          style: const TextStyle(color: LivekitColors.livekitDesignStandardFlowkitWhite, fontSize: 12),
        ),
        2.verticalSpace,
        Text(
          LiveKitLocalizations.of(Global.appContext())!.livekit_common_gift_income,
          style: const TextStyle(color: LivekitColors.livekitDesignStandardG5, fontSize: 12),
        ),
      ],
    );
  }

  _buildGiftGiversWidget() {
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      crossAxisAlignment: CrossAxisAlignment.center,
      children: [
        Text(
          "${liveController.getRoomSate().liveExtraInfo.giftPeopleSet.length}",
          style: const TextStyle(color: LivekitColors.livekitDesignStandardFlowkitWhite, fontSize: 12),
        ),
        2.verticalSpace,
        Text(
          LiveKitLocalizations.of(Global.appContext())!.livekit_common_send_gift_people_count,
          style: const TextStyle(color: LivekitColors.livekitDesignStandardG5, fontSize: 12),
        ),
      ],
    );
  }

  _buildLikesWidget() {
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      crossAxisAlignment: CrossAxisAlignment.center,
      children: [
        Text(
          "${liveController.getRoomSate().liveExtraInfo.likeCount}",
          style: const TextStyle(color: LivekitColors.livekitDesignStandardFlowkitWhite, fontSize: 12),
        ),
        2.verticalSpace,
        Text(
          LiveKitLocalizations.of(Global.appContext())!.livekit_common_like_count,
          style: const TextStyle(color: LivekitColors.livekitDesignStandardG5, fontSize: 12),
        ),
      ],
    );
  }
}

extension AnchorDashboardWidgetStateLogicExtension on AnchorDashboardWidgetState {
  void _closeWidget() {
    Navigator.of(context).pop();
  }
}
