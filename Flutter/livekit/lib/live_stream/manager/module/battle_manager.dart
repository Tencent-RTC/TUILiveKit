import 'dart:ui';
import 'dart:async';

import 'package:rtc_room_engine/api/extension/tui_live_battle_manager.dart';

import '../../../common/language/index.dart';
import '../../../common/widget/index.dart';
import '../../api/live_stream_service.dart';
import '../../state/battle_state.dart';
import '../live_stream_manager.dart';

class BattleManager {
  LSBattleState battleState = LSBattleState();

  late final Context context;
  late final LiveStreamService service;
  Timer? _timer;

  void init(Context context) {
    this.context = context;
    service = context.service;
  }

  void dispose() {}

  void resetState() {
    battleState.battleId.value = '';
    battleState.battleUsers.value = [];
    battleState.receivedBattleRequest.value = null;
    battleState.durationCountDown.value = 0;
    battleState.battleConfig = BattleConfig();
    battleState.needResponse = true;
    battleState.isInWaiting.value = false;
    battleState.isShowingStartWidget = false;
    battleState.isBattleRunning.value = false;
    battleState.isOnDisplayResult.value = false;
  }

  void onRequestBattle(String battleId, List<TUIBattleUser> battleUserList) {
    battleState.battleId.value = battleId;
    battleState.isInWaiting.value = true;
  }

  void onCanceledBattle() {
    battleState.battleId.value = '';
    battleState.isInWaiting.value = false;
  }

  void onResponseBattle() {
    battleState.receivedBattleRequest.value = null;
  }

  void onBattleExited() {
    battleState.battleId.value = '';
  }

  bool isBattleDraw() {
    BattleUser? firstUser = battleState.battleUsers.value.firstOrNull;
    BattleUser? lastUser = battleState.battleUsers.value.lastOrNull;
    if (firstUser == null || lastUser == null) {
      return false;
    }
    return firstUser.ranking == lastUser.ranking;
  }
}

extension BattlleManagerCallback on BattleManager {
  void onBattleStarted(TUIBattleInfo battleInfo) {
    battleInfo.config.duration = battleInfo.config.duration +
        battleInfo.startTime -
        (DateTime.now().millisecondsSinceEpoch ~/ 1000);

    battleState.battleId.value = battleInfo.battleId;
    battleState.isBattleRunning.value = true;
    battleState.isInWaiting.value = false;
    battleState.isShowingStartWidget = true;
    battleState.battleConfig = BattleConfig(
        duration: battleInfo.config.duration,
        needResponse: battleInfo.config.needResponse,
        extensionInfo: battleInfo.config.extensionInfo);
    battleState.durationCountDown.value = battleInfo.config.duration;

    _startCountDown();
    Future.delayed(const Duration(milliseconds: 500), () {
      battleState.isShowingStartWidget = false;
    });

    final battleUsers = battleInfo.inviteeList
        .map((battleUser) => BattleUser.fromTUIBattleUser(battleUser))
        .toList();
    battleUsers.add(BattleUser.fromTUIBattleUser(battleInfo.inviter));
    _sortedBattleUsersByScore(battleUsers);
  }

  void onBattleEnded(TUIBattleInfo battleInfo) {
    final battleUsers = battleInfo.inviteeList
        .map((battleUser) => BattleUser.fromTUIBattleUser(battleUser))
        .toList();
    battleUsers.add(BattleUser.fromTUIBattleUser(battleInfo.inviter));

    _sortedBattleUsersByScore(battleState.battleUsers.value);

    battleState.durationCountDown.value = 0;
    battleState.isOnDisplayResult.value = true;
    battleState.isBattleRunning.value = false;

    _stopCountDown();

    Future.delayed(const Duration(seconds: 5), () {
      battleState.isOnDisplayResult.value = false;
      resetState();
    });
  }

  void onUserJoinBattle(String battleId, TUIBattleUser battleUser) {
    if (battleId != battleState.battleId.value) {
      return;
    }
    final newBattleUsers = battleState.battleUsers.value.toList();
    newBattleUsers.add(BattleUser.fromTUIBattleUser(battleUser));
    battleState.battleUsers.value = newBattleUsers;
  }

  void onUserExitBattle(String battleId, TUIBattleUser battleUser) {
    if (battleState.battleUsers.value.length == 2) {
      return;
    }

    if (battleState.battleUsers.value
        .any((user) => user.userId == battleUser.userId)) {
      final newBattleUsers = battleState.battleUsers.value.toList();
      newBattleUsers.removeWhere((user) => user.userId == battleUser.userId);
      battleState.battleUsers.value = newBattleUsers;
    }

    _sortedBattleUsersByScore(battleState.battleUsers.value);
  }

  void onBattleScoreChanged(
      String battleId, List<TUIBattleUser> battleUserList) {
    _sortedBattleUsersByScore(battleUserList
        .map((battleUser) => BattleUser.fromTUIBattleUser(battleUser))
        .toList());
  }

  void onBattleRequestReceived(
      String battleId, TUIBattleUser inviter, TUIBattleUser invitee) {
    battleState.battleId.value = battleId;
    battleState.receivedBattleRequest.value =
        (battleId, BattleUser.fromTUIBattleUser(inviter));
  }

  void onBattleRequestCancelled(
      String battleId, TUIBattleUser inviter, TUIBattleUser invitee) {
    battleState.receivedBattleRequest.value = null;

    final toast = inviter.userName +
        LiveKitLocalizations.of(Global.appContext())!
            .common_battle_inviter_cancel;
    context.toastSubject.target?.add(toast);
  }

  void onBattleRequestTimeout(
      String battleId, TUIBattleUser inviter, TUIBattleUser invitee) {
    if (battleState.receivedBattleRequest.value?.$1 == battleId) {
      battleState.receivedBattleRequest.value = null;
    }
    battleState.isInWaiting.value = false;

    final toast = LiveKitLocalizations.of(Global.appContext())!
        .common_battle_invitation_timeout;
    context.toastSubject.target?.add(toast);
  }

  void onBattleRequestAccept(
      String battleId, TUIBattleUser inviter, TUIBattleUser invitee) {
    if (context.coreBattleState.inviteeList.value.isEmpty) {
      battleState.isInWaiting.value = false;
    }
  }

  void onBattleRequestReject(
      String battleId, TUIBattleUser inviter, TUIBattleUser invitee) {
    if (context.coreBattleState.inviteeList.value.isEmpty) {
      battleState.isInWaiting.value = false;
    }

    final toast = invitee.userName +
        LiveKitLocalizations.of(Global.appContext())!
            .common_battle_invitee_reject;
    context.toastSubject.target?.add(toast);
  }
}

extension on BattleManager {
  void _startCountDown() {
    _timer = Timer.periodic(const Duration(seconds: 1), (timer) {
      if (battleState.durationCountDown.value > 0) {
        battleState.durationCountDown.value -= 1;
      } else {
        _timer?.cancel();
      }
    });
  }

  void _stopCountDown() {
    _timer?.cancel();
  }

  void _sortedBattleUsersByScore(List<BattleUser> battleUsers) {
    if (battleUsers.isEmpty) {
      return;
    }
    // 1. Sort with score
    battleUsers.sort((a, b) => b.score.compareTo(a.score));

    // 2. If the second and subsequent shares are the same, the ranking is the same as the previous one, otherwise it is equal to the current number + 1
    List<BattleUser> finalUsers = [];
    for (int index = 0; index < battleUsers.length; index++) {
      BattleUser updatedUser = battleUsers[index];
      if (index > 0 && updatedUser.score == battleUsers[index - 1].score) {
        updatedUser.ranking = battleUsers[index - 1].ranking;
      } else {
        updatedUser.ranking = index + 1;
      }

      finalUsers.add(updatedUser);
    }

    battleState.battleUsers.value = finalUsers;
  }
}
