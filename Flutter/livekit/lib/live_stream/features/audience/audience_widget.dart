import 'package:flutter/material.dart';

import 'package:live_stream_core/live_core_widget/live_core_widget.dart';

import 'package:live_uikit_barrage/live_uikit_barrage.dart';
import 'package:live_uikit_gift/live_uikit_gift.dart';
import 'package:rtc_room_engine/api/common/tui_common_define.dart';
import 'package:tencent_live_uikit/common/error/error_handler.dart';
import 'package:tencent_live_uikit/common/logger/logger.dart';
import 'package:tencent_live_uikit/common/widget/global.dart';
import 'package:tencent_live_uikit/common/widget/toast.dart';
import 'package:tencent_live_uikit/live_navigator_observer.dart';
import 'package:tencent_live_uikit/live_stream/features/audience/dashboard_widget/audience_dashboard_widget.dart';
import 'package:tencent_live_uikit/live_stream/features/audience/living_widget/audience_living_widget.dart';
import 'package:tencent_live_uikit/live_stream/features/decorations/battle/battle_info_widget.dart';
import 'package:tencent_live_uikit/live_stream/features/decorations/battle/battle_member_info_widget.dart';
import 'package:tencent_live_uikit/live_stream/features/decorations/co_guest/co_guest_widget.dart';
import 'package:tencent_live_uikit/live_stream/features/decorations/co_host/co_host_widget.dart';
import 'package:tencent_live_uikit/live_stream/manager/live_stream_manager.dart';
import 'package:tencent_live_uikit/live_stream/live_define.dart'
    as video_room_live_define;

import '../../../common/resources/index.dart';

class AudienceWidget extends StatefulWidget {
  final String roomId;
  final LiveCoreController liveCoreController;
  final LiveStreamManager liveStreamManager;

  const AudienceWidget({
    super.key,
    required this.roomId,
    required this.liveCoreController,
    required this.liveStreamManager,
  });

  @override
  State<AudienceWidget> createState() => _AudienceWidgetState();
}

class _AudienceWidgetState extends State<AudienceWidget> {
  late final VoidCallback _liveStatusListener = _onLiveStatusChange;

  @override
  void initState() {
    LiveKitLogger.info("AudienceWidget init");
    super.initState();
    _init();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      resizeToAvoidBottomInset: false,
      body: PopScope(
        canPop: false,
        child: Container(
          color: LiveColors.notStandardBlack,
          child: Stack(
            children: [
              LiveCoreWidget(
                controller: widget.liveCoreController,
                videoWidgetBuilder:
                    VideoWidgetBuilder(coGuestWidgetBuilder: (context, info) {
                  return CoGuestWidget(
                    userInfo: info,
                    liveCoreController: widget.liveCoreController,
                    liveStreamManager: widget.liveStreamManager,
                  );
                }, coHostWidgetBuilder: (context, coHostUser) {
                  return Container(
                    color: Colors.transparent,
                    child: CoHostWidget(
                        coHostUser: coHostUser,
                        liveCoreController: widget.liveCoreController,
                        liveStreamManager: widget.liveStreamManager),
                  );
                }, battleWidgetBuilder: (context, battleUser) {
                  return BattleMemberInfoWidget(
                      liveStreamManager: widget.liveStreamManager,
                      battleUserId: battleUser.userId);
                }, battleContainerWidgetBuilder: (context, battleModels) {
                  _onBattleModelsChanged(battleModels);
                  return BattleInfoWidget(
                      liveStreamManager: widget.liveStreamManager,
                      isOwner: false);
                }),
              ),
              AudienceLivingWidget(
                liveCoreController: widget.liveCoreController,
                liveStreamManager: widget.liveStreamManager,
              ),
              ValueListenableBuilder(
                valueListenable: widget.liveStreamManager.roomState.liveStatus,
                builder: (BuildContext context, value, Widget? child) {
                  return Visibility(
                    visible:
                        widget.liveStreamManager.roomState.liveStatus.value ==
                            video_room_live_define.LiveStatus.finished,
                    child: AudienceDashboardWidget(
                      liveCoreController: widget.liveCoreController,
                      liveStreamManager: widget.liveStreamManager,
                    ),
                  );
                },
              ),
            ],
          ),
        ),
      ),
    );
  }

  @override
  void dispose() {
    LiveKitLogger.info("AudienceWidget dispose");
    _dispose();
    super.dispose();
  }
}

extension on _AudienceWidgetState {
  void _init() {
    _joinLiveStream();
    _addLiveStatusListener();
  }

  void _dispose() {
    _removeLiveStatusListener();
    _leaveLiveStream();
    _resetControllers();
  }

  void _joinLiveStream() async {
    var result = await widget.liveCoreController.joinLiveStream(widget.roomId);
    if (result.code != TUIError.success) {
      makeToast(
          msg: ErrorHandler.convertToErrorMessage(
                  result.code.rawValue, result.message) ??
              '');
      Navigator.pop(Global.appContext());
      return;
    }
    widget.liveStreamManager.onJoinLive(result.data!);
  }

  void _leaveLiveStream() {
    widget.liveCoreController.leaveLiveStream();
  }

  void _addLiveStatusListener() {
    widget.liveCoreController.roomState.liveStatus
        .addListener(_liveStatusListener);
  }

  void _removeLiveStatusListener() {
    widget.liveCoreController.roomState.liveStatus
        .removeListener(_liveStatusListener);
  }

  void _resetControllers() {
    BarrageDisplayController.resetState();
    GiftDisplayController.resetState();
  }

  void _onLiveStatusChange() {
    final status = widget.liveStreamManager.roomState.liveStatus.value;
    if (status == video_room_live_define.LiveStatus.finished) {
      TUILiveKitNavigatorObserver.instance.backToLiveRoomAudiencePage();
    }
  }

  void _onBattleModelsChanged(List<BattleUserWidgetModel> battleModels) {
    final battleUsers = widget.liveStreamManager.battleState.battleUsers.value;
    for (final battleModel in battleModels) {
      for (int index = 0; index < battleUsers.length; index++) {
        final battleUser = battleUsers[index];
        if (battleUser.userId == battleModel.battleUser.userId) {
          widget.liveStreamManager.battleManager
              .updateBattleUserRectFromIndex(battleModel.rect, index);
        }
      }
    }
  }
}
