import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';

import '../../../manager/live_stream_manager.dart';

class BattleMemberInfoWidget extends StatefulWidget {
  final LiveStreamManager liveStreamManager;
  final String battleUserId;
  final ValueListenable<bool> isFloatWindowMode;

  const BattleMemberInfoWidget(
      {super.key, required this.liveStreamManager, required this.battleUserId, required this.isFloatWindowMode});

  @override
  State<BattleMemberInfoWidget> createState() => _BattleMemberInfoWidgetState();
}

class _BattleMemberInfoWidgetState extends State<BattleMemberInfoWidget> {
  @override
  void dispose() {
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return ValueListenableBuilder(
      valueListenable: widget.isFloatWindowMode,
      builder: (context, isFloatWindowMode, child) {
        return Visibility(visible: !isFloatWindowMode, child: buildContent(context));
      },
    );
  }

  Widget buildContent(BuildContext context) {
    return Stack(
      children: [
        Positioned(
            top: 8.height,
            left: 8.width,
            child: ListenableBuilder(
                listenable: Listenable.merge([
                  widget.liveStreamManager.battleState.isBattleRunning,
                  widget.liveStreamManager.coHostState.connectedUsers,
                  widget.liveStreamManager.battleState.isOnDisplayResult
                ]),
                builder: (context, _) {
                  final isBattleRunning = widget.liveStreamManager.battleState.isBattleRunning.value;
                  final isOnDisplayResult = widget.liveStreamManager.battleState.isOnDisplayResult.value;
                  final isMultiplePeopleMode = widget.liveStreamManager.coHostState.connectedUsers.value.length >= 3;
                  return Visibility(
                      visible: (isBattleRunning || isOnDisplayResult) && isMultiplePeopleMode,
                      child: Container(
                        child: _buildScoreWidget(),
                      ));
                }))
      ],
    );
  }

  Widget _buildScoreWidget() {
    return Container(
      height: 28.height,
      decoration: BoxDecoration(borderRadius: BorderRadius.circular(14.height), color: LiveColors.userNameBlackColor),
      child: Padding(
        padding: EdgeInsets.only(left: 4.width, right: 4.width, top: 5.height, bottom: 5.height),
        child: Row(
          children: [
            Image.asset(getRankingImageNamesById(widget.battleUserId), package: Constants.pluginName),
            SizedBox(width: 2.width),
            Text(
              '${getScoreById(widget.battleUserId)}',
              style: const TextStyle(fontSize: 12, color: Colors.white),
            )
          ],
        ),
      ),
    );
  }

  String getRankingImageNamesById(String battleUserId) {
    String imageName = LiveImages.battleRanking1;
    final battleUser =
        widget.liveStreamManager.battleState.battleUsers.value.where((user) => user.userId == battleUserId).firstOrNull;
    if (battleUser == null) {
      return imageName;
    }

    switch (battleUser.ranking) {
      case 1:
        imageName = LiveImages.battleRanking1;
        break;
      case 2:
        imageName = LiveImages.battleRanking2;
        break;
      case 3:
        imageName = LiveImages.battleRanking3;
        break;
      case 4:
        imageName = LiveImages.battleRanking4;
        break;
      case 5:
        imageName = LiveImages.battleRanking5;
        break;
      case 6:
        imageName = LiveImages.battleRanking6;
        break;
      case 7:
        imageName = LiveImages.battleRanking7;
        break;
      case 8:
        imageName = LiveImages.battleRanking8;
        break;
      case 9:
        imageName = LiveImages.battleRanking9;
        break;
      default:
        break;
    }
    return imageName;
  }

  int getScoreById(String battleUserId) {
    final battleUser =
        widget.liveStreamManager.battleState.battleUsers.value.where((user) => user.userId == battleUserId).firstOrNull;
    return battleUser?.score ?? 0;
  }
}
