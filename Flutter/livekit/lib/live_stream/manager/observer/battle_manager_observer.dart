import 'package:live_stream_core/live_core_widget/index.dart';
import 'package:tencent_live_uikit/live_stream/manager/module/battle_manager.dart';

import '../live_stream_manager.dart';

class BattleManagerObserver extends BattleObserver {
  late final Context context;

  void init(Context context) {
    this.context = context;
  }

  BattleManagerObserver() {
    super.onBattleStarted = (battleInfo) {
      context.battleManager.target?.onBattleStarted(battleInfo);
    };
    super.onBattleEnded = (battleInfo) {
      context.battleManager.target?.onBattleEnded(battleInfo);
    };
    super.onUserJoinBattle = (battleId, battleUser) {
      context.battleManager.target?.onUserJoinBattle(battleId, battleUser);
    };
    super.onUserExitBattle = (battleId, battleUser) {
      context.battleManager.target?.onUserExitBattle(battleId, battleUser);
    };
    super.onBattleScoreChanged = (battleId, battleUserList) {
      context.battleManager.target
          ?.onBattleScoreChanged(battleId, battleUserList);
    };
    super.onBattleRequestReceived = (battleId, inviter, invitee) {
      context.battleManager.target
          ?.onBattleRequestReceived(battleId, inviter, invitee);
    };
    super.onBattleRequestCancelled = (battleId, inviter, invitee) {
      context.battleManager.target
          ?.onBattleRequestCancelled(battleId, inviter, invitee);
    };
    super.onBattleRequestTimeout = (battleId, inviter, invitee) {
      context.battleManager.target
          ?.onBattleRequestTimeout(battleId, inviter, invitee);
    };
    super.onBattleRequestAccept = (battleId, inviter, invitee) {
      context.battleManager.target
          ?.onBattleRequestAccept(battleId, inviter, invitee);
    };
    super.onBattleRequestReject = (battleId, inviter, invitee) {
      context.battleManager.target
          ?.onBattleRequestReject(battleId, inviter, invitee);
    };
  }
}
