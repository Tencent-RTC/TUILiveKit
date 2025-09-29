import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/live_core_widget.dart';
import 'package:live_uikit_barrage/live_uikit_barrage.dart';
import 'package:live_uikit_gift/live_uikit_gift.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/live_navigator_observer.dart';
import 'package:tencent_live_uikit/live_stream/features/audience/living_widget/audience_empty_seat_widget.dart';
import 'package:tencent_live_uikit/live_stream/features/audience/living_widget/audience_living_widget.dart';
import 'package:tencent_live_uikit/live_stream/features/decorations/index.dart';
import 'package:tencent_live_uikit/live_stream/features/index.dart';
import 'package:tencent_live_uikit/live_stream/live_define.dart' as video_room_live_define;
import 'package:tencent_live_uikit/live_stream/manager/live_stream_manager.dart';

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
          color: LiveColors.notStandardPureBlack,
          child: Stack(
            children: [
              LiveCoreWidget(
                controller: widget.liveCoreController,
                videoWidgetBuilder: VideoWidgetBuilder(coGuestWidgetBuilder: (context, seatFullInfo, viewLayer) {
                  if (seatFullInfo.userId.isEmpty) {
                    if (viewLayer == ViewLayer.background) {
                      return AudienceEmptySeatWidget(
                          seatFullInfo: seatFullInfo,
                          liveCoreController: widget.liveCoreController,
                          liveStreamManager: widget.liveStreamManager);
                    }
                    return Container();
                  }
                  if (viewLayer == ViewLayer.background) {
                    return CoGuestBackgroundWidget(
                        userInfo: seatFullInfo, liveCoreController: widget.liveCoreController);
                  } else {
                    return CoGuestForegroundWidget(
                        userInfo: seatFullInfo, liveCoreController: widget.liveCoreController);
                  }
                }, coHostWidgetBuilder: (context, seatFullInfo, viewLayer) {
                  if (viewLayer == ViewLayer.background) {
                    return CoHostBackgroundWidget(
                        userInfo: seatFullInfo, liveCoreController: widget.liveCoreController);
                  } else {
                    return CoHostForegroundWidget(
                        userInfo: seatFullInfo, liveCoreController: widget.liveCoreController);
                  }
                }, battleWidgetBuilder: (context, battleUser) {
                  return BattleMemberInfoWidget(
                      liveStreamManager: widget.liveStreamManager, battleUserId: battleUser.userId);
                }, battleContainerWidgetBuilder: (context, seatList) {
                  return BattleInfoWidget(
                      seatList: seatList, liveStreamManager: widget.liveStreamManager, isOwner: false);
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
                    visible: widget.liveStreamManager.roomState.liveStatus.value ==
                        video_room_live_define.LiveStatus.finished,
                    child: AudienceEndStatisticsWidget(
                      roomId: widget.roomId,
                      avatarUrl: widget.liveCoreController.roomState.ownerInfo.avatarUrl,
                      userName: widget.liveCoreController.roomState.ownerInfo.userName,
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
    var result = await widget.liveCoreController.joinLiveStreamV2(widget.roomId);
    if (result.code != TUIError.success) {
      makeToast(msg: ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
      Navigator.pop(Global.appContext());
      return;
    }
    widget.liveStreamManager.onJoinLive(result.data!);
  }

  void _leaveLiveStream() {
    widget.liveCoreController.leaveLiveStream();
  }

  void _addLiveStatusListener() {
    widget.liveCoreController.roomState.liveStatus.addListener(_liveStatusListener);
  }

  void _removeLiveStatusListener() {
    widget.liveCoreController.roomState.liveStatus.removeListener(_liveStatusListener);
  }

  void _resetControllers() {
    BarrageDisplayController.resetState();
    GiftPlayController.resetState();
  }

  void _onLiveStatusChange() {
    final status = widget.liveStreamManager.roomState.liveStatus.value;
    if (status == video_room_live_define.LiveStatus.finished) {
      TUILiveKitNavigatorObserver.instance.backToLiveRoomAudiencePage();
    }
  }
}
