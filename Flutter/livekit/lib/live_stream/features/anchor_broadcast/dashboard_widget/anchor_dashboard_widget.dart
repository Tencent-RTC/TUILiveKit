import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/index.dart';
import 'package:tencent_live_uikit/common/index.dart';

import '../../../manager/live_stream_manager.dart';

class AnchorDashboardWidget extends StatefulWidget {
  final LiveStreamManager liveStreamManager;
  final LiveCoreController liveCoreController;

  const AnchorDashboardWidget(
      {super.key,
      required this.liveStreamManager,
      required this.liveCoreController});

  @override
  State<AnchorDashboardWidget> createState() => _AnchorDashboardWidgetState();
}

class _AnchorDashboardWidgetState extends State<AnchorDashboardWidget> {
  late final LiveStreamManager liveStreamManager;
  late final LiveCoreController liveCoreController;

  @override
  void initState() {
    super.initState();
    liveStreamManager = widget.liveStreamManager;
    liveCoreController = widget.liveCoreController;
  }

  @override
  Widget build(BuildContext context) {
    return Container(
      color: LiveColors.designStandardG2,
      child: Stack(
        alignment: Alignment.topCenter,
        children: [
          _buildBackWidget(),
          _buildTitleWidget(),
          _buildDataWidget(),
        ],
      ),
    );
  }

  Widget _buildBackWidget() {
    return Positioned(
      right: 16.width,
      top: 58.height,
      width: 24.width,
      height: 24.width,
      child: GestureDetector(
        onTap: () {
          _closeWidget();
        },
        child: Image.asset(
          LiveImages.audienceClose,
          package: Constants.pluginName,
        ),
      ),
    );
  }

  Widget _buildTitleWidget() {
    return Positioned(
      top: 120.height,
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

  Widget _buildDataWidget() {
    return Positioned(
      top: 200.height,
      child: Container(
        width: 345.width,
        height: 95.height,
        decoration: BoxDecoration(
          color: LiveColors.notStandardBlue30Transparency,
          borderRadius: BorderRadius.all(Radius.circular(20.radius)),
        ),
        child: Column(
          mainAxisAlignment: MainAxisAlignment.start,
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Container(
              margin: EdgeInsets.only(top: 5.height, left: 15.width),
              child: Text(
                LiveKitLocalizations.of(Global.appContext())!
                    .common_common_this_live_data,
                style: const TextStyle(
                    color: LiveColors.designStandardFlowkitWhite, fontSize: 14),
              ),
            ),
            SizedBox(
              height: 70.height,
              child: Row(
                mainAxisAlignment: MainAxisAlignment.spaceAround,
                crossAxisAlignment: CrossAxisAlignment.center,
                children: [
                  _buildDurationWidget(),
                  _buildViewersWidget(),
                ],
              ),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildDurationWidget() {
    int duration = ((DateTime.now().millisecondsSinceEpoch -
                liveStreamManager.roomState.createTime) ~/
            1000)
        .abs();
    if (liveStreamManager.roomState.createTime == 0) {
      duration = 0;
    }
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      crossAxisAlignment: CrossAxisAlignment.center,
      children: [
        Text(
          getFormatDuration(duration),
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

  Widget _buildViewersWidget() {
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      crossAxisAlignment: CrossAxisAlignment.center,
      children: [
        Text(
          '${liveStreamManager.roomState.liveExtraInfo.maxAudienceCount}',
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

  Widget _buildMessageWidget() {
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      crossAxisAlignment: CrossAxisAlignment.center,
      children: [
        Text(
          '${liveStreamManager.roomState.liveExtraInfo.messageCount}',
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

  Widget _buildGiftIncomeWidget() {
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      crossAxisAlignment: CrossAxisAlignment.center,
      children: [
        Text(
          '${liveStreamManager.roomState.liveExtraInfo.giftIncome}',
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

  Widget _buildGiftGiversWidget() {
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      crossAxisAlignment: CrossAxisAlignment.center,
      children: [
        Text(
          '${liveStreamManager.roomState.liveExtraInfo.giftPeopleSet.length}',
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

  Widget _buildLikesWidget() {
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      crossAxisAlignment: CrossAxisAlignment.center,
      children: [
        Text(
          '${liveStreamManager.roomState.liveExtraInfo.likeCount}',
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

  String getFormatDuration(int duration) {
    if (duration <= 0) return "--:--";

    int h = duration ~/ 3600;
    int m = (duration % 3600) ~/ 60;
    int s = duration % 60;

    return [
      h.toString().padLeft(2, '0'),
      m.toString().padLeft(2, '0'),
      s.toString().padLeft(2, '0')
    ].join(':');
  }
}
