import 'dart:async';

import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/live_core_widget.dart';
import 'package:live_uikit_barrage/live_uikit_barrage.dart';
import 'package:live_uikit_gift/live_uikit_gift.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/live_navigator_observer.dart';
import 'package:tencent_live_uikit/live_stream/features/audience/living_widget/audience_empty_seat_widget.dart';
import 'package:tencent_live_uikit/live_stream/features/audience/living_widget/audience_living_widget.dart';
import 'package:tencent_live_uikit/live_stream/features/audience/panel/audience_user_info_panel_widget.dart';
import 'package:tencent_live_uikit/live_stream/features/audience/panel/audience_user_management_panel_widget.dart';
import 'package:tencent_live_uikit/live_stream/features/decorations/index.dart';
import 'package:tencent_live_uikit/live_stream/features/index.dart';
import 'package:tencent_live_uikit/live_stream/live_define.dart' as video_room_live_define;
import 'package:tencent_live_uikit/live_stream/manager/live_stream_manager.dart';

import '../../../common/gesture/gesture_mock.dart';
import '../../../component/float_window/global_float_window_manager.dart';

class AudienceWidget extends StatefulWidget {
  final String roomId;
  final LiveCoreController liveCoreController;
  final LiveStreamManager liveStreamManager;
  final VoidCallback? onTapEnterFloatWindowInApp;

  const AudienceWidget(
      {super.key,
      required this.roomId,
      required this.liveCoreController,
      required this.liveStreamManager,
      this.onTapEnterFloatWindowInApp});

  @override
  State<AudienceWidget> createState() => _AudienceWidgetState();
}

class _AudienceWidgetState extends State<AudienceWidget> {
  late final VoidCallback _liveStatusListener = _onLiveStatusChange;
  late final VoidCallback _audioLockedListener = _onAudioLockedStatusChanged;
  late final VoidCallback _videoLockedListener = _onVideoLockedStatusChanged;
  late final StreamSubscription<void> _kickedOutSubscription;

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
                        userInfo: seatFullInfo,
                        liveCoreController: widget.liveCoreController,
                        isFloatWindowMode: widget.liveStreamManager.floatWindowState.isFloatWindowMode);
                  } else {
                    return GestureDetector(
                      onTap: () {
                        final isSelf = widget.liveStreamManager.coreUserState.selfInfo.userId == seatFullInfo.userId;
                        final isOwner = widget.liveStreamManager.coreRoomState.ownerInfo.userId == seatFullInfo.userId;
                        final user = TUIUserInfo(
                            userId: seatFullInfo.userId,
                            userName: seatFullInfo.userName,
                            avatarUrl: seatFullInfo.userAvatar,
                            userRole: isOwner ? TUIRole.roomOwner : TUIRole.generalUser);
                        if (!isSelf) {
                          popupWidget(
                              AudienceUserInfoPanelWidget(user: user, liveStreamManager: widget.liveStreamManager),
                              backgroundColor: LiveColors.designStandardTransparent);
                        } else {
                          popupWidget(AudienceUserManagementPanelWidget(
                            user: user,
                            liveStreamManager: widget.liveStreamManager,
                            liveCoreController: widget.liveCoreController,
                          ));
                        }
                      },
                      child: Container(
                        color: Colors.transparent,
                        child: CoGuestForegroundWidget(
                            userInfo: seatFullInfo,
                            liveCoreController: widget.liveCoreController,
                            isFloatWindowMode: widget.liveStreamManager.floatWindowState.isFloatWindowMode),
                      ),
                    );
                  }
                }, coHostWidgetBuilder: (context, seatFullInfo, viewLayer) {
                  if (viewLayer == ViewLayer.background) {
                    return CoHostBackgroundWidget(
                        userInfo: seatFullInfo,
                        liveCoreController: widget.liveCoreController,
                        isFloatWindowMode: widget.liveStreamManager.floatWindowState.isFloatWindowMode);
                  } else {
                    return CoHostForegroundWidget(
                        userInfo: seatFullInfo,
                        liveCoreController: widget.liveCoreController,
                        isFloatWindowMode: widget.liveStreamManager.floatWindowState.isFloatWindowMode);
                  }
                }, battleWidgetBuilder: (context, battleUser) {
                  return BattleMemberInfoWidget(
                      liveStreamManager: widget.liveStreamManager,
                      battleUserId: battleUser.userId,
                      isFloatWindowMode: widget.liveStreamManager.floatWindowState.isFloatWindowMode);
                }, battleContainerWidgetBuilder: (context, seatList) {
                  return BattleInfoWidget(
                      seatList: seatList,
                      liveStreamManager: widget.liveStreamManager,
                      isOwner: false,
                      isFloatWindowMode: widget.liveStreamManager.floatWindowState.isFloatWindowMode);
                }),
              ),
              AudienceLivingWidget(
                liveCoreController: widget.liveCoreController,
                liveStreamManager: widget.liveStreamManager,
                onTapEnterFloatWindowInApp: widget.onTapEnterFloatWindowInApp,
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
    _kickedOutSubscription = widget.liveStreamManager.kickedOutSubject.stream.listen((_) => _handleKickedOut());
    _joinLiveStream();
    _addLiveStatusListener();
    _addMediaLockedListener();
  }

  void _dispose() {
    _kickedOutSubscription.cancel();
    _removeMediaLockedListener();
    _removeLiveStatusListener();
    _leaveLiveStream();
    _resetControllers();
  }

  void _handleKickedOut() {
    makeToast(msg: LiveKitLocalizations.of(Global.appContext())!.common_kicked_out_of_room_by_owner);
    _closePage();
  }

  void _closePage() {
    GestureMock.mockTapEvent();
    if (GlobalFloatWindowManager.instance.isEnableFloatWindowFeature()) {
      GlobalFloatWindowManager.instance.overlayManager.closeOverlay();
    } else {
      Future.delayed(const Duration(milliseconds: 100), () {
        Navigator.pop(Global.appContext());
      });
    }
  }

  void _joinLiveStream() async {
    var result = await widget.liveCoreController.joinLiveStreamV2(widget.roomId);
    if (result.code != TUIError.success) {
      makeToast(msg: ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
      _closePage();
      return;
    }
    widget.liveStreamManager.onJoinLive(result.data!);
    widget.liveStreamManager.resumeByAudience();
  }

  void _leaveLiveStream() {
    widget.liveCoreController.leaveLiveStream();
  }

  void _addLiveStatusListener() {
    widget.liveStreamManager.roomState.liveStatus.addListener(_liveStatusListener);
  }

  void _removeLiveStatusListener() {
    widget.liveStreamManager.roomState.liveStatus.removeListener(_liveStatusListener);
  }

  void _addMediaLockedListener() {
    widget.liveStreamManager.mediaState.isAudioLocked.addListener(_audioLockedListener);
    widget.liveStreamManager.mediaState.isVideoLocked.addListener(_videoLockedListener);
  }

  void _removeMediaLockedListener() {
    widget.liveStreamManager.mediaState.isAudioLocked.removeListener(_audioLockedListener);
    widget.liveStreamManager.mediaState.isVideoLocked.removeListener(_videoLockedListener);
  }

  void _resetControllers() {
    BarrageDisplayController.resetState();
    GiftPlayController.resetState();
  }

  void _onLiveStatusChange() {
    final status = widget.liveStreamManager.roomState.liveStatus.value;
    if (status == video_room_live_define.LiveStatus.finished) {
      GlobalFloatWindowManager floatWindowManager = GlobalFloatWindowManager.instance;
      if (floatWindowManager.isEnableFloatWindowFeature()) {
        GestureMock.mockTapEvent();
        if (floatWindowManager.isFloating()) {
          floatWindowManager.overlayManager.closeOverlay();
        }
      } else {
        TUILiveKitNavigatorObserver.instance.backToLiveRoomAudiencePage();
      }
    }
  }

  void _onAudioLockedStatusChanged() {
    final isAudioLocked = widget.liveStreamManager.mediaState.isAudioLocked.value;
    final toastMessage = isAudioLocked
        ? LiveKitLocalizations.of(context)!.common_mute_audio_by_master
        : LiveKitLocalizations.of(context)!.common_un_mute_audio_by_master;
    makeToast(msg: toastMessage);
  }

  void _onVideoLockedStatusChanged() {
    final isVideoLocked = widget.liveStreamManager.mediaState.isVideoLocked.value;
    final toastMessage = isVideoLocked
        ? LiveKitLocalizations.of(context)!.common_mute_video_by_owner
        : LiveKitLocalizations.of(context)!.common_un_mute_video_by_master;
    makeToast(msg: toastMessage);
  }
}
