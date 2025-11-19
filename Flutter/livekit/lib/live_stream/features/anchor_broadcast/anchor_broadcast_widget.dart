import 'dart:async';

import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/index.dart' hide LiveStatus;
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/live_stream/features/anchor_broadcast/co_guest/anchor_empty_seat_widget.dart';
import 'package:tencent_live_uikit/live_stream/features/anchor_broadcast/living_widget/anchor_user_management_panel_widget.dart';
import 'package:tencent_live_uikit/live_stream/features/index.dart';
import 'package:tencent_live_uikit/live_stream/state/battle_state.dart';

import '../../live_define.dart';
import '../../manager/live_stream_manager.dart';
import '../decorations/index.dart';
import 'battle/battle_count_down_widget.dart';
import 'living_widget/anchor_living_widget.dart';

class AnchorBroadcastWidget extends StatefulWidget {
  final LiveStreamManager liveStreamManager;
  final LiveCoreController liveCoreController;
  final VoidCallback? onTapEnterFloatWindowInApp;

  const AnchorBroadcastWidget(
      {super.key, required this.liveStreamManager, required this.liveCoreController, this.onTapEnterFloatWindowInApp});

  @override
  State<AnchorBroadcastWidget> createState() => _AnchorBroadcastWidgetState();
}

class _AnchorBroadcastWidgetState extends State<AnchorBroadcastWidget> {
  late final LiveStreamManager liveStreamManager;
  late final LiveCoreController liveCoreController;
  late final StreamSubscription<String> _toastSubscription;
  late final StreamSubscription<void> _kickedOutSubscription;
  late final VoidCallback _connectionRequestListener = _handleConnectionRequest;
  late final VoidCallback _battleRequestListener = _handleBattleRequest;
  late final VoidCallback _battleWaitingStatusListener = _handleBattleWaitingStatusChanged;
  late final VoidCallback _isFloatWindowModeListener = _isFloatWindowModeChanged;
  bool isShowingConnectRequestAlert = false;
  bool isShowingBattleRequestAlert = false;
  bool isShowingBattleWaitingSheet = false;

  @override
  void initState() {
    super.initState();
    liveStreamManager = widget.liveStreamManager;
    liveCoreController = widget.liveCoreController;
    _addObserver();
  }

  @override
  void dispose() {
    _removeObserver();
    super.dispose();
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
            children: [_buildCoreWidget(), _buildLivingWidget(), _buildDashboardWidget()],
          ),
        ),
      ),
    );
  }

  Widget _buildCoreWidget() {
    final isFloatWindowMode = liveStreamManager.floatWindowState.isFloatWindowMode.value;
    return Padding(
      padding: isFloatWindowMode ? EdgeInsets.zero : EdgeInsets.only(top: 44.height, bottom: 96.height),
      child: ClipRRect(
        borderRadius: isFloatWindowMode ? BorderRadius.zero : BorderRadius.circular(16.radius),
        child: LiveCoreWidget(
          controller: liveCoreController,
          videoWidgetBuilder: VideoWidgetBuilder(coGuestWidgetBuilder: (context, seatFullInfo, viewLayer) {
            if (seatFullInfo.userId.isEmpty) {
              if (viewLayer == ViewLayer.background) {
                return AnchorEmptySeatWidget(seatFullInfo: seatFullInfo, liveStreamManager: liveStreamManager);
              } else {
                return Container();
              }
            }
            if (viewLayer == ViewLayer.background) {
              return CoGuestBackgroundWidget(
                  userInfo: seatFullInfo,
                  liveCoreController: widget.liveCoreController,
                  isFloatWindowMode: liveStreamManager.floatWindowState.isFloatWindowMode);
            } else {
              return GestureDetector(
                  onTap: () => _onTapCoGuestForegroundWidget(seatFullInfo),
                  child: Container(
                      color: Colors.transparent,
                      child: CoGuestForegroundWidget(
                          userInfo: seatFullInfo,
                          liveCoreController: widget.liveCoreController,
                          isFloatWindowMode: widget.liveStreamManager.floatWindowState.isFloatWindowMode)));
            }
          }, coHostWidgetBuilder: (context, seatFullInfo, viewLayer) {
            if (viewLayer == ViewLayer.background) {
              return CoHostBackgroundWidget(
                  userInfo: seatFullInfo,
                  liveCoreController: widget.liveCoreController,
                  isFloatWindowMode: liveStreamManager.floatWindowState.isFloatWindowMode);
            } else {
              return CoHostForegroundWidget(
                  userInfo: seatFullInfo,
                  liveCoreController: widget.liveCoreController,
                  isFloatWindowMode: widget.liveStreamManager.floatWindowState.isFloatWindowMode);
            }
          }, battleWidgetBuilder: (context, battleUserInfo) {
            return BattleMemberInfoWidget(
                liveStreamManager: liveStreamManager,
                battleUserId: battleUserInfo.userId,
                isFloatWindowMode: widget.liveStreamManager.floatWindowState.isFloatWindowMode);
          }, battleContainerWidgetBuilder: (context, seatList) {
            return BattleInfoWidget(
                seatList: seatList,
                liveStreamManager: liveStreamManager,
                isOwner: true,
                isFloatWindowMode: widget.liveStreamManager.floatWindowState.isFloatWindowMode);
          }),
        ),
      ),
    );
  }

  Widget _buildLivingWidget() {
    return AnchorLivingWidget(
      liveStreamManager: liveStreamManager,
      liveCoreController: liveCoreController,
      onTapEnterFloatWindowInApp: widget.onTapEnterFloatWindowInApp,
    );
  }

  Widget _buildDashboardWidget() {
    return ValueListenableBuilder(
        valueListenable: liveStreamManager.roomState.liveStatus,
        builder: (context, liveStatus, _) {
          int liveDuration =
              ((DateTime.now().millisecondsSinceEpoch - liveStreamManager.roomState.createTime) ~/ 1000).abs();
          if (liveStreamManager.roomState.createTime == 0) {
            liveDuration = 0;
          }
          var endInfo = AnchorEndStatisticsWidgetInfo(
              roomId: liveStreamManager.roomState.roomId,
              liveDuration: liveDuration,
              viewCount: liveStreamManager.roomState.liveExtraInfo.maxAudienceCount,
              messageCount: liveStreamManager.roomState.liveExtraInfo.messageCount,
              giftTotalCoins: liveStreamManager.roomState.liveExtraInfo.giftIncome,
              giftTotalUniqueSender: liveStreamManager.roomState.liveExtraInfo.giftPeopleSet.length,
              likeTotalUniqueSender: liveStreamManager.roomState.liveExtraInfo.likeCount);
          return Visibility(
            visible: liveStatus == LiveStatus.finished,
            child: AnchorEndStatisticsWidget(
              endWidgetInfo: endInfo,
            ),
          );
        });
  }
}

extension on _AnchorBroadcastWidgetState {
  void _addObserver() {
    liveStreamManager.coreCoHostState.receivedConnectionRequest.addListener(_connectionRequestListener);
    liveStreamManager.battleState.receivedBattleRequest.addListener(_battleRequestListener);
    liveStreamManager.battleState.isInWaiting.addListener(_battleWaitingStatusListener);
    liveStreamManager.floatWindowState.isFloatWindowMode.addListener(_isFloatWindowModeListener);

    _toastSubscription = liveStreamManager.toastSubject.stream.listen((toast) => makeToast(msg: toast));
    _kickedOutSubscription = liveStreamManager.kickedOutSubject.stream.listen((_) => _handleKickedOut());
  }

  void _removeObserver() {
    liveStreamManager.coreCoHostState.receivedConnectionRequest.removeListener(_connectionRequestListener);
    liveStreamManager.battleState.receivedBattleRequest.removeListener(_battleRequestListener);
    liveStreamManager.battleState.isInWaiting.removeListener(_battleWaitingStatusListener);
    liveStreamManager.floatWindowState.isFloatWindowMode.removeListener(_isFloatWindowModeListener);

    _toastSubscription.cancel();
    _kickedOutSubscription.cancel();
  }

  void _handleConnectionRequest() {
    if (liveStreamManager.coreCoHostState.receivedConnectionRequest.value == null && isShowingConnectRequestAlert) {
      Navigator.of(Global.appContext()).pop();
      isShowingConnectRequestAlert = false;
      return;
    }
    if (liveStreamManager.floatWindowState.isFloatWindowMode.value) {
      return;
    }
    if (liveStreamManager.coreCoHostState.receivedConnectionRequest.value != null && !isShowingConnectRequestAlert) {
      final inviter = liveStreamManager.coreCoHostState.receivedConnectionRequest.value!;
      final alertInfo = AlertInfo(
          imageUrl: inviter.avatarUrl,
          description: LiveKitLocalizations.of(Global.appContext())!
              .common_connect_inviting_append
              .replaceAll("xxx", inviter.userName),
          cancelActionInfo: (
            title: LiveKitLocalizations.of(Global.appContext())!.common_reject,
            titleColor: LiveColors.designStandardG3
          ),
          cancelCallback: () {
            _responseCoHostInvitation(inviter, false);
          },
          defaultActionInfo: (
            title: LiveKitLocalizations.of(Global.appContext())!.common_accept,
            titleColor: LiveColors.designStandardB1
          ),
          defaultCallback: () {
            _responseCoHostInvitation(inviter, true);
          });
      Alert.showAlert(alertInfo);
      isShowingConnectRequestAlert = true;
    }
  }

  void _responseCoHostInvitation(TUIConnectionUser inviter, bool isAccepted) async {
    isShowingConnectRequestAlert = false;
    liveCoreController.respondToCrossRoomConnection(inviter.roomId, isAccepted).then((result) {
      if (result.code != TUIError.success) {
        liveStreamManager.toastSubject
            .add(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
      }
    });

    if (mounted) Navigator.of(Global.appContext()).pop();
  }

  void _handleBattleRequest() {
    if (liveStreamManager.battleState.receivedBattleRequest.value == null && isShowingBattleRequestAlert) {
      Navigator.of(Global.appContext()).pop();
      isShowingBattleRequestAlert = false;
      return;
    }
    if (liveStreamManager.floatWindowState.isFloatWindowMode.value) {
      return;
    }
    if (liveStreamManager.battleState.receivedBattleRequest.value != null && !isShowingBattleRequestAlert) {
      final battleId = liveStreamManager.battleState.receivedBattleRequest.value!.$1;
      final inviter = liveStreamManager.battleState.receivedBattleRequest.value!.$2;
      final alertInfo = AlertInfo(
          imageUrl: inviter.avatarUrl,
          description:
              LiveKitLocalizations.of(Global.appContext())!.common_battle_inviting.replaceAll("xxx", inviter.userName),
          cancelActionInfo: (
            title: LiveKitLocalizations.of(Global.appContext())!.common_reject,
            titleColor: LiveColors.designStandardG3
          ),
          cancelCallback: () {
            _responseBattleInvitation(battleId, false);
          },
          defaultActionInfo: (
            title: LiveKitLocalizations.of(Global.appContext())!.common_receive,
            titleColor: LiveColors.designStandardB1
          ),
          defaultCallback: () {
            _responseBattleInvitation(battleId, true);
          });

      Alert.showAlert(alertInfo);
      isShowingBattleRequestAlert = true;
    }
  }

  void _responseBattleInvitation(String battleId, bool isAccepted) async {
    isShowingBattleRequestAlert = false;
    liveStreamManager.onResponseBattle();
    liveCoreController.respondToBattle(battleId, isAccepted).then((result) {
      if (result.code != TUIError.success) {
        liveStreamManager.toastSubject
            .add(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
      }
    });

    if (mounted) Navigator.of(Global.appContext()).pop();
  }

  void _handleBattleWaitingStatusChanged() {
    if (!liveStreamManager.battleState.isInWaiting.value && isShowingBattleWaitingSheet) {
      Navigator.of(Global.appContext()).pop();
      isShowingBattleWaitingSheet = false;
      return;
    }

    if (liveStreamManager.battleState.isInWaiting.value && !isShowingBattleWaitingSheet) {
      popupWidget(
          BattleCountDownWidget(
            isFloatWindowMode: liveStreamManager.floatWindowState.isFloatWindowMode,
            countdownTime: LSBattleState.battleRequestTime,
            onCancel: () async {
              final inviteeIdList =
                  liveStreamManager.coreBattleState.inviteeList.value.map((user) => user.userId).toList();
              liveCoreController.cancelBattle(liveStreamManager.battleState.battleId.value, inviteeIdList);
              liveStreamManager.onCanceledBattle();
            },
            onTimeEnd: () {
              liveStreamManager.onCanceledBattle();
            },
          ),
          backgroundColor: Colors.transparent,
          isDismissible: false, onDismiss: () {
        isShowingBattleWaitingSheet = false;
      });
      isShowingBattleWaitingSheet = true;
    }
  }

  void _handleKickedOut() {}

  void _isFloatWindowModeChanged() {
    if (liveStreamManager.floatWindowState.isFloatWindowMode.value) {
      if (isShowingConnectRequestAlert) {
        Navigator.of(Global.appContext()).pop();
        isShowingConnectRequestAlert = false;
      }
      if (isShowingBattleRequestAlert) {
        Navigator.of(Global.appContext()).pop();
        isShowingBattleRequestAlert = false;
      }
    } else {
      _handleConnectionRequest();
      _handleBattleRequest();
    }
  }

  void _onTapCoGuestForegroundWidget(SeatFullInfo seatFullInfo) {
    final isOwner = widget.liveStreamManager.coreRoomState.ownerInfo.userId == seatFullInfo.userId;
    final isSelf = widget.liveStreamManager.coreUserState.selfInfo.userId == seatFullInfo.userId;
    final user = TUIUserInfo(
        userId: seatFullInfo.userId,
        userName: seatFullInfo.userName,
        avatarUrl: seatFullInfo.userAvatar,
        userRole: isOwner ? TUIRole.roomOwner : TUIRole.generalUser);
    popupWidget(AnchorUserManagementPanelWidget(
      panelType: isSelf ? AnchorUserManagementPanelType.pureMedia : AnchorUserManagementPanelType.mediaAndSeat,
      user: user,
      liveStreamManager: liveStreamManager,
      liveCoreController: liveCoreController,
    ));
  }
}
