import 'dart:async';

import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/index.dart' hide LiveStatus;
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_live_uikit/common/screen/index.dart';
import 'package:tencent_live_uikit/live_stream/state/battle_state.dart';

import '../../../common/error/index.dart';
import '../../../common/language/index.dart';
import '../../../common/resources/index.dart';
import '../../../common/widget/index.dart';
import '../../live_define.dart';
import '../../manager/live_stream_manager.dart';
import '../decorations/index.dart';
import 'battle/battle_count_down_widget.dart';
import 'living_widget/anchor_living_widget.dart';
import 'dashboard_widget/anchor_dashboard_widget.dart';

class AnchorBroadcastWidget extends StatefulWidget {
  final LiveStreamManager liveStreamManager;
  final LiveCoreController liveCoreController;

  const AnchorBroadcastWidget(
      {super.key,
      required this.liveStreamManager,
      required this.liveCoreController});

  @override
  State<AnchorBroadcastWidget> createState() => _AnchorBroadcastWidgetState();
}

class _AnchorBroadcastWidgetState extends State<AnchorBroadcastWidget> {
  late final LiveStreamManager liveStreamManager;
  late final LiveCoreController liveCoreController;
  late final StreamSubscription<String> _toastSubscription;
  late final StreamSubscription<void> _kickedOutSubscription;
  bool isShowingAlert = false;

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
          color: LiveColors.notStandardBlack,
          child: Stack(
            children: [
              _buildCoreWidget(),
              _buildLivingWidget(),
              _buildDashboardWidget()
            ],
          ),
        ),
      ),
    );
  }

  Widget _buildCoreWidget() {
    return Padding(
      padding: EdgeInsets.only(top: 36.height, bottom: 96.height),
      child: ClipRRect(
        borderRadius: BorderRadius.circular(16.radius),
        child: LiveCoreWidget(
          controller: liveCoreController,
          videoWidgetBuilder:
              VideoWidgetBuilder(coGuestWidgetBuilder: (context, coGuestUser) {
            return CoGuestWidget(
                userInfo: coGuestUser,
                liveCoreController: liveCoreController,
                liveStreamManager: liveStreamManager);
          }, coHostWidgetBuilder: (context, coHostUserInfo) {
            return CoHostWidget(
                coHostUser: coHostUserInfo,
                liveStreamManager: liveStreamManager,
                liveCoreController: liveCoreController);
          }, battleWidgetBuilder: (context, battleUserInfo) {
            return BattleMemberInfoWidget(
                liveStreamManager: liveStreamManager,
                battleUserId: battleUserInfo.userId);
          }, battleContainerWidgetBuilder: (context, battleModels) {
            _onBattleModelsChanged(battleModels);
            return BattleInfoWidget(
                liveStreamManager: liveStreamManager, isOwner: true);
          }),
        ),
      ),
    );
  }

  Widget _buildLivingWidget() {
    return AnchorLivingWidget(
        liveStreamManager: liveStreamManager,
        liveCoreController: liveCoreController);
  }

  Widget _buildDashboardWidget() {
    return ValueListenableBuilder(
        valueListenable: liveStreamManager.roomState.liveStatus,
        builder: (context, liveStatus, _) {
          return Visibility(
              visible: liveStatus == LiveStatus.finished,
              child: AnchorDashboardWidget(
                  liveStreamManager: liveStreamManager,
                  liveCoreController: liveCoreController));
        });
  }
}

extension on _AnchorBroadcastWidgetState {
  void _addObserver() {
    liveStreamManager.coreCoHostState.receivedConnectionRequest
        .addListener(_handleConnectionRequest);
    liveStreamManager.battleState.receivedBattleRequest
        .addListener(_handleBattleRequest);
    liveStreamManager.battleState.isInWaiting
        .addListener(_handleBattleWaitingStatusChanged);

    _toastSubscription = liveStreamManager.toastSubject.stream
        .listen((toast) => makeToast(msg: toast));
    _kickedOutSubscription = liveStreamManager.kickedOutSubject.stream
        .listen((_) => _handleKickedOut());
  }

  void _removeObserver() {
    liveStreamManager.coreCoHostState.receivedConnectionRequest
        .removeListener(_handleConnectionRequest);
    liveStreamManager.battleState.receivedBattleRequest
        .removeListener(_handleBattleRequest);
    liveStreamManager.battleState.isInWaiting
        .removeListener(_handleBattleWaitingStatusChanged);

    _toastSubscription.cancel();
    _kickedOutSubscription.cancel();
  }

  void _handleConnectionRequest() {
    if (liveStreamManager.coreCoHostState.receivedConnectionRequest.value ==
            null &&
        isShowingAlert) {
      Navigator.of(context).pop();
      isShowingAlert = false;
      return;
    }

    if (liveStreamManager.coreCoHostState.receivedConnectionRequest.value !=
            null &&
        !isShowingAlert) {
      final inviter =
          liveStreamManager.coreCoHostState.receivedConnectionRequest.value!;
      final alertInfo = AlertInfo(
          imageUrl: inviter.avatarUrl,
          description: LiveKitLocalizations.of(Global.appContext())!
              .common_connect_inviting_append
              .replaceAll('xxx', inviter.userName),
          cancelActionInfo: (
            title: LiveKitLocalizations.of(Global.appContext())!.common_reject,
            titleColor: LiveColors.designStandardG3
          ),
          cancelCallback: () {
            _responseCoHostInvitation(inviter, false);
            isShowingAlert = false;
          },
          defaultActionInfo: (
            title: LiveKitLocalizations.of(Global.appContext())!.common_accept,
            titleColor: LiveColors.designStandardB1
          ),
          defaultCallback: () {
            _responseCoHostInvitation(inviter, true);
            isShowingAlert = false;
          });

      Alert.showAlert(alertInfo);
      isShowingAlert = true;
    }
  }

  void _responseCoHostInvitation(
      TUIConnectionUser inviter, bool isAccepted) async {
    liveCoreController
        .respondToCrossRoomConnection(inviter.roomId, isAccepted)
        .then((result) {
      if (result.code != TUIError.success) {
        liveStreamManager.toastSubject.add(ErrorHandler.convertToErrorMessage(
                result.code.rawValue, result.message) ??
            '');
      }
    });

    if (mounted) Navigator.of(context).pop();
  }

  void _handleBattleRequest() {
    if (liveStreamManager.battleState.receivedBattleRequest.value == null &&
        isShowingAlert) {
      Navigator.of(context).pop();
      isShowingAlert = false;
      return;
    }

    if (liveStreamManager.battleState.receivedBattleRequest.value != null &&
        !isShowingAlert) {
      final battleId =
          liveStreamManager.battleState.receivedBattleRequest.value!.$1;
      final inviter =
          liveStreamManager.battleState.receivedBattleRequest.value!.$2;
      final alertInfo = AlertInfo(
          imageUrl: inviter.avatarUrl,
          description: LiveKitLocalizations.of(Global.appContext())!
              .common_battle_inviting
              .replaceAll('xxx', inviter.userName),
          cancelActionInfo: (
            title: LiveKitLocalizations.of(Global.appContext())!.common_reject,
            titleColor: LiveColors.designStandardG3
          ),
          cancelCallback: () {
            _responseBattleInvitation(battleId, false);
            isShowingAlert = false;
          },
          defaultActionInfo: (
            title: LiveKitLocalizations.of(Global.appContext())!.common_accept,
            titleColor: LiveColors.designStandardB1
          ),
          defaultCallback: () {
            _responseBattleInvitation(battleId, true);
            isShowingAlert = false;
          });

      Alert.showAlert(alertInfo);
      isShowingAlert = true;
    }
  }

  void _responseBattleInvitation(String battleId, bool isAccepted) async {
    liveCoreController.respondToBattle(battleId, isAccepted).then((result) {
      if (result.code != TUIError.success) {
        liveStreamManager.toastSubject.add(ErrorHandler.convertToErrorMessage(
                result.code.rawValue, result.message) ??
            '');
      }
    });

    if (mounted) Navigator.of(context).pop();
  }

  void _handleBattleWaitingStatusChanged() {
    if (!liveStreamManager.battleState.isInWaiting.value && isShowingAlert) {
      Navigator.of(context).pop();
      isShowingAlert = false;
      return;
    }

    if (liveStreamManager.battleState.isInWaiting.value && !isShowingAlert) {
      popupWidget(
          BattleCountDownWidget(
            countdownTime: LSBattleState.battleRequestTime,
            onCancel: () async {
              final inviteeIdList = liveStreamManager
                  .coreBattleState.inviteeList.value
                  .map((user) => user.userId)
                  .toList();
              liveCoreController.cancelBattle(
                  liveStreamManager.battleState.battleId.value, inviteeIdList);
              liveStreamManager.onCanceledBattle();
            },
            onTimeEnd: () {
              liveStreamManager.onCanceledBattle();
            },
          ),
          backgroundColor: Colors.transparent);
      isShowingAlert = true;
    }
  }

  void _handleKickedOut() {}

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
