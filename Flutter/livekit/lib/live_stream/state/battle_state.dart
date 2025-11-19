import 'package:flutter/material.dart';
import 'package:rtc_room_engine/api/extension/tui_live_battle_manager.dart';

class LSBattleState {
  static const int battleDuration = 30;
  static const int battleRequestTime = 10;
  static const int battleEndInfoDuration = 5;

  final ValueNotifier<String> battleId = ValueNotifier('');
  final ValueNotifier<List<BattleUser>> battleUsers = ValueNotifier([]);
  final ValueNotifier<ReceivedBattleRequest?> receivedBattleRequest = ValueNotifier(null);
  final ValueNotifier<int> durationCountDown = ValueNotifier(0);
  BattleConfig battleConfig = BattleConfig();
  bool needResponse = true;

  final ValueNotifier<bool> isInWaiting = ValueNotifier(false);
  bool isShowingStartWidget = false;
  final ValueNotifier<bool> isBattleRunning = ValueNotifier(false);
  final ValueNotifier<bool> isOnDisplayResult = ValueNotifier(false);
}

typedef ReceivedBattleRequest = (String battleId, BattleUser inviter);

class BattleUser {
  String roomId;
  String userId;
  String avatarUrl;
  String userName;
  int score;
  int ranking;

  BattleUser(
      {this.roomId = '',
      this.userId = '',
      this.avatarUrl = '',
      this.userName = '',
      this.score = 0,
      this.ranking = 1});

  BattleUser.fromTUIBattleUser(TUIBattleUser battleUser)
      : roomId = battleUser.roomId,
        userId = battleUser.userId,
        avatarUrl = battleUser.avatarUrl,
        userName = battleUser.userName,
        score = battleUser.score,
        ranking = 1;

  @override
  String toString() {
    return 'BattleUser{roomId: $roomId, userId: $userId, avatarUrl: $avatarUrl, userName: $userName, score: $score, ranking: $ranking}';
  }
}

class BattleConfig {
  int duration;
  bool needResponse;
  String extensionInfo;

  BattleConfig(
      {this.duration = 30, this.needResponse = true, this.extensionInfo = ''});
}
