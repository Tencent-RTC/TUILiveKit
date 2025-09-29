import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';

import 'end_statistics_widget_define.dart';

class AnchorEndStatisticsWidget extends StatefulWidget {
  final AnchorEndStatisticsWidgetInfo endWidgetInfo;

  const AnchorEndStatisticsWidget({
    super.key,
    required this.endWidgetInfo,
  });

  @override
  State<AnchorEndStatisticsWidget> createState() => _AnchorEndStatisticsWidgetState();
}

class _AnchorEndStatisticsWidgetState extends State<AnchorEndStatisticsWidget> {
  @override
  void initState() {
    super.initState();
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
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      crossAxisAlignment: CrossAxisAlignment.center,
      children: [
        Text(
          getFormatDuration(widget.endWidgetInfo.liveDuration),
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
          '${widget.endWidgetInfo.viewCount}',
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

  // ignore: unused_element
  Widget _buildMessageWidget() {
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      crossAxisAlignment: CrossAxisAlignment.center,
      children: [
        Text(
          '${widget.endWidgetInfo.messageCount}',
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

  // ignore: unused_element
  Widget _buildGiftIncomeWidget() {
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      crossAxisAlignment: CrossAxisAlignment.center,
      children: [
        Text(
          '${widget.endWidgetInfo.giftTotalCoins}',
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

  // ignore: unused_element
  Widget _buildGiftGiversWidget() {
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      crossAxisAlignment: CrossAxisAlignment.center,
      children: [
        Text(
          '${widget.endWidgetInfo.giftTotalUniqueSender}',
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

  // ignore: unused_element
  Widget _buildLikesWidget() {
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      crossAxisAlignment: CrossAxisAlignment.center,
      children: [
        Text(
          '${widget.endWidgetInfo.likeTotalUniqueSender}',
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

extension on _AnchorEndStatisticsWidgetState {
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
