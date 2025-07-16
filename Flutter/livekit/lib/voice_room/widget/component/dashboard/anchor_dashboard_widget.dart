import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';

import '../../../../common/constants/index.dart';
import '../../../../common/language/index.dart';
import '../../../../common/resources/index.dart';
import '../../../../common/widget/index.dart';

class AnchorDashboardWidget extends StatefulWidget {
  final LiveDataModel model;

  const AnchorDashboardWidget({super.key, required this.model});

  @override
  State<AnchorDashboardWidget> createState() => _AnchorDashboardWidgetState();
}

class _AnchorDashboardWidgetState extends State<AnchorDashboardWidget> {
  late final LiveDataModel model;
  late double _screenWidth;

  @override
  void initState() {
    super.initState();
    model = widget.model;
  }

  @override
  Widget build(BuildContext context) {
    _screenWidth = MediaQuery.of(context).size.width;
    return Container(
      color: LiveColors.designStandardG2,
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
          LiveImages.close,
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
          LiveKitLocalizations.of(Global.appContext())!.common_live_has_stop,
          style: const TextStyle(
              color: LiveColors.designStandardFlowkitWhite, fontSize: 20),
        ),
      ),
    );
  }

  _initDataWidget() {
    return Positioned(
      top: 200,
      child: Container(
        width: _screenWidth - 30,
        height: 165,
        decoration: const BoxDecoration(
          color: LiveColors.notStandardBlue30Transparency,
          borderRadius: BorderRadius.all(Radius.circular(20)),
        ),
        child: Column(
          mainAxisAlignment: MainAxisAlignment.start,
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Container(
              margin: const EdgeInsets.only(top: 5, left: 15),
              child: Text(
                LiveKitLocalizations.of(Global.appContext())!
                    .common_common_this_live_data,
                style: const TextStyle(
                    color: LiveColors.designStandardFlowkitWhite, fontSize: 14),
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
          "${model.liveDuration}",
          style: const TextStyle(
              color: LiveColors.designStandardFlowkitWhite, fontSize: 12),
        ),
        SizedBox(height: 2.height),
        Text(
          LiveKitLocalizations.of(Global.appContext())!
              .common_common_live_duration,
          style:
              const TextStyle(color: LiveColors.designStandardG5, fontSize: 12),
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
          "${model.audienceCount}",
          style: const TextStyle(
              color: LiveColors.designStandardFlowkitWhite, fontSize: 12),
        ),
        SizedBox(height: 2.height),
        Text(
          LiveKitLocalizations.of(Global.appContext())!
              .common_common_live_people_number,
          style:
              const TextStyle(color: LiveColors.designStandardG5, fontSize: 12),
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
          "${model.messageCount}",
          style: const TextStyle(
              color: LiveColors.designStandardFlowkitWhite, fontSize: 12),
        ),
        SizedBox(height: 2.height),
        Text(
          LiveKitLocalizations.of(Global.appContext())!
              .common_common_message_count,
          style:
              const TextStyle(color: LiveColors.designStandardG5, fontSize: 12),
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
          "${model.giftIncome}",
          style: const TextStyle(
              color: LiveColors.designStandardFlowkitWhite, fontSize: 12),
        ),
        SizedBox(height: 2.height),
        Text(
          LiveKitLocalizations.of(Global.appContext())!
              .common_common_gift_income,
          style:
              const TextStyle(color: LiveColors.designStandardG5, fontSize: 12),
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
          "${model.giftPeopleCount}",
          style: const TextStyle(
              color: LiveColors.designStandardFlowkitWhite, fontSize: 12),
        ),
        SizedBox(height: 2.height),
        Text(
          LiveKitLocalizations.of(Global.appContext())!
              .common_common_send_gift_people_count,
          style:
              const TextStyle(color: LiveColors.designStandardG5, fontSize: 12),
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
          "${model.likeCount}",
          style: const TextStyle(
              color: LiveColors.designStandardFlowkitWhite, fontSize: 12),
        ),
        SizedBox(height: 2.height),
        Text(
          LiveKitLocalizations.of(Global.appContext())!
              .common_common_like_count,
          style:
              const TextStyle(color: LiveColors.designStandardG5, fontSize: 12),
        ),
      ],
    );
  }
}

extension on _AnchorDashboardWidgetState {
  void _closeWidget() {
    Navigator.of(context).pop();
  }
}

class LiveDataModel {
  final String roomId;
  final int liveDuration;
  final int audienceCount;
  final int messageCount;
  final int giftIncome;
  final int giftPeopleCount;
  final int likeCount;

  LiveDataModel(
      {required this.roomId,
      required this.liveDuration,
      required this.audienceCount,
      required this.messageCount,
      required this.giftIncome,
      required this.giftPeopleCount,
      required this.likeCount});
}
