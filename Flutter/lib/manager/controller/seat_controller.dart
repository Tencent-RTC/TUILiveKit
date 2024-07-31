import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/manager/index.dart';
import 'package:tencent_live_uikit/state/index.dart';

class SeatController extends Controller {
  static const String tag = "SeatController";
  static const int timeout = 60;

  SeatController(super.state, super.service);

  @override
  void destroy() {
    LiveKitLogger.info("$tag destroy");
  }

  Future<void> getSeatList() async {
    var getSeatResult = await liveService.getSeatList();
    if (getSeatResult.code != TUIError.success) {
      LiveKitLogger.error("$tag getSeatList [code:${getSeatResult.code},message:${getSeatResult.message}]");
      return;
    }
    if (getSeatResult.data == null) {
      LiveKitLogger.error("$tag getSeatList [list:${getSeatResult.data}]");
      return;
    }
    seatState.updateSeatList(getSeatResult.data!);
    updateSelfSeatedState();
    autoTakeSeatByOwner();
  }

  void takeSeat(int index) {
    if (needRequestToTakeSeat()) {
      viewState.linkStatus.value = LinkStatus.applying;
    }
    TUIRequest request = liveService.takeSeat(
        index,
        timeout,
        TUIRequestCallback(
          onAccepted: (String requestId, String userId) {
            seatState.mySeatApplicationId.value = "";
          },
          onRejected: (String requestId, String userId, String message) {
            seatState.mySeatApplicationId.value = "";
            viewState.linkStatus.value = LinkStatus.none;
            makeToast(msg: LiveKitLocalizations.of(Global.appContext())!.livekit_take_seat_rejected);
          },
          onCancelled: (String requestId, String userId) {
            seatState.mySeatApplicationId.value = "";
            viewState.linkStatus.value = LinkStatus.none;
          },
          onTimeout: (String requestId, String userId) {
            seatState.mySeatApplicationId.value = "";
            viewState.linkStatus.value = LinkStatus.none;
            makeToast(msg: LiveKitLocalizations.of(Global.appContext())!.livekit_take_seat_timeout);
          },
          onError: (String requestId, String userId, TUIError error, String message) {
            seatState.mySeatApplicationId.value = "";
            viewState.linkStatus.value = LinkStatus.none;
            ErrorHandler.onError(error);
          },
        ));
    seatState.mySeatApplicationId.value = request.requestId;
  }

  Future<void> leaveSeat() async {
    TUIActionCallback result = liveService.leaveSeat() as TUIActionCallback;
    if (result.code != TUIError.success) {
      LiveKitLogger.error("$tag leaveSeat [code:${result.code},message:${result.message}]");
      ErrorHandler.onError(result.code);
      return;
    }
  }

  Future<void> lockSeat(SeatInfo seatInfo) async {
    var lockParams = TUISeatLockParams();
    lockParams.lockAudio = seatInfo.isAudioLocked.value!;
    lockParams.lockSeat = !seatInfo.isLocked.value!;
    TUIActionCallback result = liveService.lockSeatByAdmin(seatInfo.index, lockParams) as TUIActionCallback;
    if (result.code != TUIError.success) {
      LiveKitLogger.error("$tag lockSeat [code:${result.code},message:${result.message}]");
      ErrorHandler.onError(result.code);
      return;
    }
  }

  Future<void> muteSeatAudio(SeatInfo seatInfo) async {
    var lockParams = TUISeatLockParams();
    lockParams.lockAudio = !seatInfo.isAudioLocked.value!;
    lockParams.lockSeat = seatInfo.isLocked.value!;
    TUIActionCallback result = liveService.lockSeatByAdmin(seatInfo.index, lockParams) as TUIActionCallback;
    if (result.code != TUIError.success) {
      LiveKitLogger.error("$tag muteSeatAudio [code:${result.code},message:${result.message}]");
      ErrorHandler.onError(result.code);
      return;
    }
  }

  Future<void> kickSeat(String userId) async {
    TUIActionCallback result = liveService.kickUserOffSeatByAdmin(0, userId) as TUIActionCallback;
    if (result.code != TUIError.success) {
      LiveKitLogger.error("$tag kickSeat [code:${result.code},message:${result.message}]");
      ErrorHandler.onError(result.code);
      return;
    }
  }

  Future<void> getSeatApplicationList() async {
    final result = await liveService.getSeatApplicationList();
    if (result.code == TUIError.success) {
      var list = result.data as List<TUIRequest>;
      seatState.initSeatApplicationList(list);
    } else {
      ErrorHandler.onError(result.code);
    }
  }

  void acceptRequest(String requestId) async {
    final result = await liveService.responseRemoteRequest(requestId, true);
    if (result.code == TUIError.success) {
      seatState.removeSeatApplication(requestId);
    } else {
      ErrorHandler.onError(result.code);
    }
  }

  void rejectRequest(String requestId) async {
    final result = await liveService.responseRemoteRequest(requestId, false);
    if (result.code == TUIError.success) {
      seatState.removeSeatApplication(requestId);
    } else {
      ErrorHandler.onError(result.code);
    }
  }

  void cancelTakeSeatApplication() {
    if (seatState.mySeatApplicationId.value.isEmpty) {
      LiveKitLogger.error("$tag getSeatApplicationList cancelTakeSeatApplication mySeatApplicationId is empty");
      return;
    }
    TUIActionCallback result = liveService.cancelRequest(seatState.mySeatApplicationId.value) as TUIActionCallback;
    if (result.code != TUIError.success) {
      LiveKitLogger.error("$tag cancelTakeSeatApplication [code:${result.code},message:${result.message}]");
      ErrorHandler.onError(result.code);
      return;
    }
    seatState.removeSeatApplication(seatState.mySeatApplicationId.value);
    seatState.mySeatApplicationId.value = "";
    viewState.linkStatus.value = LinkStatus.none;
  }

  void kickUserOffSeatByAdmin(int seatIndex, SeatInfo seatInfo) async {
    final result = await liveService.kickUserOffSeatByAdmin(seatIndex, seatInfo.userId.value);
    if (result.code == TUIError.success) {
      seatState.removeSeatInfo(seatInfo);
    } else {
      ErrorHandler.onError(result.code);
    }
  }

  void updateSelfSeatedState() {
    if (isSelfInSeat()) {
      viewState.linkStatus.value = LinkStatus.linking;
    }
  }

  void autoTakeSeatByOwner() {
    if (userState.selfInfo.role.value != TUIRole.roomOwner) {
      return;
    }
    if (viewState.linkStatus.value != LinkStatus.linking) {
      takeSeat(0);
    }
  }

  bool isSelfInSeat() {
    var selfUserid = userState.selfInfo.userId;
    if (selfUserid.isEmpty) {
      return false;
    }
    var seatInfo = SeatInfo();
    seatInfo.userId.value = selfUserid;
    return seatState.seatList.value.contains(seatInfo);
  }

  bool needRequestToTakeSeat() {
    var role = userState.selfInfo.role.value;
    if (role == TUIRole.roomOwner || role == TUIRole.administrator) {
      return false;
    }
    return roomState.seatMode.value == TUISeatMode.applyToTake;
  }

  bool isSelfSeatInfo(SeatInfo seatInfo) {
    if (userState.selfInfo.userId.isEmpty) {
      return false;
    }
    return userState.selfInfo.userId == seatInfo.userId.value;
  }

  void onSeatListChanged(List<TUISeatInfo> seatList, List<TUISeatInfo> seatedList, List<TUISeatInfo> leftList) {
    seatState.updateSeatList(seatList);
    for (var info in seatedList) {
      SeatInfo seatInfo = SeatInfo();
      seatInfo.updateState(info);
      if (isSelfSeatInfo(seatInfo)) {
        viewState.linkStatus.value = LinkStatus.linking;
      }
    }

    for (var info in leftList) {
      SeatInfo seatInfo = SeatInfo();
      seatInfo.updateState(info);
      if (isSelfSeatInfo(seatInfo)) {
        viewState.linkStatus.value = LinkStatus.none;
      }
    }
  }

  void onRequestReceived(TUIRequest request) {
    if (request.requestAction != TUIRequestAction.requestToTakeSeat) {
      return;
    }
    seatState.addSeatApplication(request);
  }

  void onRequestCancelled(TUIRequest request, TUIUserInfo userInfo) {
    seatState.removeSeatApplication(request.requestId);
  }

  void onRequestProcessed(TUIRequest request, TUIUserInfo userInfo) {
    seatState.removeSeatApplication(request.requestId);
  }

  void onKickedOffSeat(seatIndex, TUIUserInfo operateUser) {
    makeToast(msg: LiveKitLocalizations.of(Global.appContext())!.livekit_kicked_out_of_seat);
  }
}
