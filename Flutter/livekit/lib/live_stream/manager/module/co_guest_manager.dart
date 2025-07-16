import 'package:rtc_room_engine/api/common/tui_common_define.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:tencent_live_uikit/common/index.dart';

import '../../api/live_stream_service.dart';
import '../../state/co_guest_state.dart';
import '../live_stream_manager.dart';

class CoGuestManager {
  LSCoGuestState coGuestState = LSCoGuestState();

  late final Context context;
  late final LiveStreamService service;

  void init(Context context) {
    this.context = context;
    service = context.service;
    _subscribeCoreCoGuestState();
  }

  void dispose() {
    _unSubscribeCoreCoGuestState();
  }

  Future<TUIActionCallback> onLockMediaStatusBtnClicked(
      String userId, TUISeatLockParams params) async {
    final seatIndex = context.coreCoGuestState.seatList.value
        .where((seat) => seat.userId == userId)
        .firstOrNull
        ?.index;

    return seatIndex != null
        ? service.lockSeatByAdmin(seatIndex!, params)
        : TUIActionCallback(
            code: TUIError.errUserNotInSeat, message: 'Not on the seat');
  }

  void onStartRequestIntraRoomConnection() {
    coGuestState.coGuestStatus.value = CoGuestStatus.applying;
  }

  void onRequestIntraRoomConnectionFailed() {
    coGuestState.coGuestStatus.value = CoGuestStatus.none;
  }

  void onUserConnectionRejected(String userId) {
    context.toastSubject.target?.add(
        LiveKitLocalizations.of(Global.appContext())!
            .common_voiceroom_take_seat_rejected);
    coGuestState.coGuestStatus.value = CoGuestStatus.none;
  }

  void onUserConnectionTimeout(String userId) {
    context.toastSubject.target?.add(
        LiveKitLocalizations.of(Global.appContext())!
            .common_voiceroom_take_seat_timeout);
    coGuestState.coGuestStatus.value = CoGuestStatus.none;
  }

  void onKickedOffSeat() {
    context.toastSubject.target?.add(
        LiveKitLocalizations.of(Global.appContext())!
            .common_voiceroom_kicked_out_of_seat);
    coGuestState.coGuestStatus.value = CoGuestStatus.none;
  }

  void onStartCancelIntraRoomConnection() {
    coGuestState.coGuestStatus.value = CoGuestStatus.none;
  }

  void onCancelIntraRoomConnection() {
    coGuestState.coGuestStatus.value = CoGuestStatus.none;
  }
}

extension on CoGuestManager {
  void _subscribeCoreCoGuestState() {
    context.coreCoGuestState.seatList.addListener(_onSeatListChanged);
  }

  void _unSubscribeCoreCoGuestState() {
    context.coreCoGuestState.seatList.removeListener(_onSeatListChanged);
  }

  void _onSeatListChanged() {
    final seatList = context.coreCoGuestState.seatList.value;
    _updateCoGuestStatusBySeatList(seatList);
    _updateMediaLockStatus(seatList);
    if (!seatList
        .any((seat) => seat.userId == context.coreUserState.selfInfo.userId)) {
      context.mediaManager.target?.onSelfLeaveSeat();
    }
  }

  void _updateCoGuestStatusBySeatList(List<TUISeatInfo> seatList) {
    final isLinking = seatList
        .any((seat) => seat.userId == context.coreUserState.selfInfo.userId);
    coGuestState.coGuestStatus.value =
        isLinking ? CoGuestStatus.linking : CoGuestStatus.none;
  }

  void _updateMediaLockStatus(List<TUISeatInfo> seatList) {
    final newLockAudioUserList = coGuestState.lockAudioUserList.value;
    final newLockVideoUSerList = coGuestState.lockVideoUserList.value;
    for (final seatInfo in seatList) {
      if (seatInfo.userId.isEmpty) {
        continue;
      }
      bool isAudioLocked = seatInfo.isAudioLocked ?? false;
      isAudioLocked
          ? newLockAudioUserList.add(seatInfo.userId)
          : newLockAudioUserList.remove(seatInfo.userId);
      coGuestState.lockAudioUserList.value = newLockAudioUserList;

      bool isVideoLocked = seatInfo.isVideoLocked ?? false;
      isVideoLocked
          ? newLockVideoUSerList.add(seatInfo.userId)
          : newLockVideoUSerList.remove(seatInfo.userId);
      coGuestState.lockVideoUserList.value = newLockVideoUSerList;

      if (seatInfo.userId == context.coreUserState.selfInfo.userId) {
        context.mediaManager.target?.onSelfMediaDeviceStateChanged(seatInfo);
      }
    }
  }
}
