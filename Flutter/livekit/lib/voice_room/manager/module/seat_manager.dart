import 'dart:async';

import 'package:rtc_room_engine/api/common/tui_common_define.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:tencent_live_uikit/common/error/error_handler.dart';

import '../../index.dart';

class SeatManager {
  final SeatState state = SeatState();
  late final Context context;
  late final VoiceRoomService service;
  late final StreamController<String>? toastSubject;

  void init(Context context) {
    this.context = context;
    service = context.service;
    toastSubject = context.toastSubject.target;
  }

  Future<void> fetchSeatList() async {
    final result = await service.fetchSeatList();
    if (result.code == TUIError.success && result.data != null) {
      state.seatList.value = result.data!;
      return;
    }
    toastSubject?.add(ErrorHandler.convertToErrorMessage(
            result.code.rawValue, result.message) ??
        '');
  }

  Future<void> fetchSeatApplicationList() async {
    final result = await service.fetchSeatApplicationList();
    if (result.code == TUIError.success && result.data != null) {
      state.seatApplicationList.value = result.data!;
      return;
    }
    toastSubject?.add(ErrorHandler.convertToErrorMessage(
            result.code.rawValue, result.message) ??
        '');
  }

  void onSeatApplicationReceived({required TUIUserInfo from}) {
    if (!state.seatApplicationList.value
        .any((seatApplication) => seatApplication.userId == from.userId)) {
      final List<SeatApplication> seatApplicationList =
          List.from(state.seatApplicationList.value);
      seatApplicationList.add(SeatApplication.fromTUIUserInfo(from));
      state.seatApplicationList.value = seatApplicationList;
    }
  }

  void onSeatApplicationProcessed({required TUIUserInfo of}) {
    final List<SeatApplication> seatApplicationList =
        List.from(state.seatApplicationList.value);
    seatApplicationList
        .removeWhere((seatApplication) => seatApplication.userId == of.userId);
    state.seatApplicationList.value = seatApplicationList;
  }

  void onSentSeatInvitation({required String to}) {
    final Set<String> newInvitedUserIds = Set.from(state.invitedUserIds.value);
    newInvitedUserIds.add(to);
    state.invitedUserIds.value = newInvitedUserIds;
  }

  void onRespondedSeatInvitation({required String of}) {
    final Set<String> invitedUserIds = Set.from(state.invitedUserIds.value);
    invitedUserIds.removeWhere((invitedUserId) => invitedUserId == of);
    state.invitedUserIds.value = invitedUserIds;
  }

  void onApplyingToSeatStateChanged({required bool isApplying}) {
    state.isApplyingToTakeSeat.value = isApplying;
  }
}

extension VoiceRoomSeatManagerCallback on SeatManager {
  void onSeatListChanged(List<TUISeatInfo> seatList,
      List<TUISeatInfo> seatedList, List<TUISeatInfo> leftList) {
    state.seatList.value =
        seatList.map((seat) => SeatInfo.fromTUISeatInfo(seat)).toList();
  }
}
