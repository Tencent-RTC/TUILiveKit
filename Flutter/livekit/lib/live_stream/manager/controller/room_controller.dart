import 'dart:convert';

import 'package:rtc_room_engine/rtc_room_engine.dart';

import '../../../common/index.dart';
import '../../state/index.dart';
import '../index.dart';

class RoomController extends Controller {
  static const String tag = 'RoomController';
  static List<String> mCategoryList = [];

  RoomController(super.state, super.service);

  @override
  void destroy() {}

  void initCreateRoomState(
      String roomId, String roomName, TUISeatMode seatMode, int maxSeatCount) {
    roomState.roomId = roomId;
    roomState.roomName.value = roomName;
    roomState.seatMode.value = seatMode;
    roomState.maxSeatCount.value = maxSeatCount;
    roomState.createTime = DateTime.now().millisecondsSinceEpoch;
    roomState.ownerInfo.userId = userState.selfInfo.userId;
    roomState.ownerInfo.name.value = userState.selfInfo.name.value;
    roomState.ownerInfo.avatarUrl.value = userState.selfInfo.avatarUrl.value;
    userState.selfInfo.role.value = TUIRole.roomOwner;
  }

  Future<TUIValueCallBack<TUIRoomInfo>> start() async {
    LiveKitLogger.info(
        "$tag start[roomId:${roomState.roomId},liveService:${liveService.hashCode}]");
    _dataReport();
    if (roomState.roomId.isEmpty) {
      LiveKitLogger.error('$tag not init create room state');
      return TUIValueCallBack(
          code: TUIError.errFailed, message: 'not init create room state');
    }

    roomState.roomName.value = getDefaultRoomName();
    viewState.liveStatus.value = LiveStatus.pushing;

    final roomInfo = TUIRoomInfo(roomId: roomState.roomId);
    roomInfo.roomType = TUIRoomType.livingRoom;
    roomInfo.isSeatEnabled = true;
    roomInfo.name = roomState.roomName.value;
    roomInfo.maxSeatCount = roomState.maxSeatCount.value;
    roomInfo.seatMode = roomState.seatMode.value;
    viewState.liveStatus.value = LiveStatus.pushing;
    final result = await liveService.start(roomInfo);
    if (result.code == TUIError.success) {
      updateRoomState(result.data);
      updateLiveInfo();
      roomState.enterRoomSuccess.value = true;
    } else {
      ErrorHandler.onError(result.code.value(), result.message);
      viewState.liveStatus.value = LiveStatus.none;
    }
    return result;
  }

  Future<TUIValueCallBack<TUIRoomInfo>> join(String roomId) async {
    LiveKitLogger.info(
        "$tag start[roomId:$roomId,liveService:${liveService.hashCode}]");
    _dataReport();
    final result = await liveService.join(roomId);
    if (result.code != TUIError.success) {
      LiveKitLogger.error(
          "$tag _initData [code:${result.code},message:${result.message}]");
      ErrorHandler.onError(result.code.value(), result.message);
    } else {
      var roomInfo = result.data as TUIRoomInfo;
      roomState.updateState(roomInfo);
      roomState.enterRoomSuccess.value = true;
    }
    return result;
  }

  Future<TUIActionCallback> exit() {
    LiveKitLogger.info(
        "$tag exit[roomId:${roomState.roomId},liveService:${liveService.hashCode}]");
    viewState.liveStatus.value = LiveStatus.dashboard;
    if (_isOwner()) {
      return _stop();
    } else {
      return _leave();
    }
  }

  void clearLiveState() {
    viewState.liveStatus.value = LiveStatus.none;
  }

  void updateMessageCount(int messageCount) {
    roomState.liveExtraInfo.messageCount = messageCount;
  }

  void updateGiftIncome(int giftIncome) {
    roomState.liveExtraInfo.giftIncome = giftIncome;
  }

  void insertGiftPeople(String userId) {
    roomState.liveExtraInfo.giftPeopleSet.add(userId);
  }

  void updateLikeNumber(int likeCount) {
    roomState.liveExtraInfo.likeCount = likeCount;
  }

  void startPreview() {
    viewState.liveStatus.value = LiveStatus.previewing;
  }

  void setRoomName(String roomName) {
    roomState.roomName.value = roomName;
  }

  void setLiveCategoryList(List<String> list) {
    if (mCategoryList.isEmpty) {
      mCategoryList.addAll(list);
    }
  }

  List<String> getLiveCategoryList() {
    return mCategoryList;
  }

  void setLivePrivacyStatus(LiveStreamPrivacyStatus liveMode) {
    roomState.liveExtraInfo.liveMode.value = liveMode;
  }

  void updateRoomState(TUIRoomInfo? roomInfo) {
    roomState.updateState(roomInfo);
  }

  String getDefaultRoomName() {
    if (roomState.roomName.value != null &&
        roomState.roomName.value!.isNotEmpty) {
      return roomState.roomName.value!;
    } else if (userState.selfInfo.name.value == null ||
        userState.selfInfo.name.value!.isEmpty) {
      return userState.selfInfo.userId;
    } else {
      return userState.selfInfo.name.value!;
    }
  }

  void updateLiveInfo() {
    TUILiveInfo liveInfo = TUILiveInfo();
    liveInfo.roomInfo = TUIRoomInfo(roomId: roomState.roomId);
    liveInfo.coverUrl = roomState.coverURL.value;
    liveInfo.isPublicVisible = LiveStreamPrivacyStatus.publicity ==
        roomState.liveExtraInfo.liveMode.value;
    liveService.setLiveInfo(
      roomState.roomId,
      coverUrl: roomState.coverURL.value,
      isPublicVisible: LiveStreamPrivacyStatus.publicity ==
          roomState.liveExtraInfo.liveMode.value,
    );
    return;
  }

  Future<TUIActionCallback> _leave() {
    return liveService.leave();
  }

  Future<TUIActionCallback> _stop() {
    return liveService.stop();
  }

  bool _isOwner() {
    final selfUserId = TUIRoomEngine.getSelfInfo().userId;
    if (selfUserId.isEmpty) {
      return false;
    }
    return selfUserId == roomState.ownerInfo.userId;
  }

  void _dataReport() {
    try {
      Map<String, dynamic> params = {
        "framework": Constants.dataReportFramework,
        "component": Constants.dataReportComponent,
        "language": Constants.dataReportLanguageFlutter,
      };

      Map<String, dynamic> jsonObject = {
        "api": "setFramework",
        "params": params,
      };

      String jsonString = jsonEncode(jsonObject);
      liveService.callExperimentalAPI(jsonString);
    } catch (e) {
      ('dataReport: $e');
    }
  }

  void onRoomUserCountChanged(String roomId, int userCount) {
    if (userCount > 0) {
      roomState.userCount.value = userCount - 1;
      if (userCount > roomState.liveExtraInfo.maxAudienceCount) {
        roomState.liveExtraInfo.maxAudienceCount = userCount - 1;
      }
    }
  }
}
