import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:rtc_room_engine/api/room/tui_room_observer.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/manager/index.dart';

class LiveObserver extends TUIRoomObserver{
  static const String tag = "LiveObserver";
  final LiveController liveController;

  LiveObserver(this.liveController) {
    super.onRoomDismissed = (roomId, reason) {
      LiveKitLogger.info("$tag onRoomDismissed:[roomId:$roomId]");
      liveController.viewController.onLiveEnd(roomId);
    };

    super.onRoomUserCountChanged = (roomId, userCount) {
      LiveKitLogger.info("$tag  onRoomUserCountChanged:[roomId:$roomId,userCount:$userCount]");
      liveController.roomController.onRoomUserCountChanged(roomId, userCount);
    };

    super.onSeatListChanged = (seatList, seatedList, leftList) {
      LiveKitLogger.info("$tag onSeatListChanged:[seatList:$seatList ,seatedList:$seatedList,leftList:$leftList]");
      liveController.seatController.onSeatListChanged(seatList, seatedList, leftList);
    };

    super.onRequestReceived = (request) {
      LiveKitLogger.info("$tag onRequestReceived:[request:$request");
      liveController.seatController.onRequestReceived(request);
    };

    super.onRequestCancelled = (requestId, userId) {
      LiveKitLogger.info("$tag onRequestCancelled:[requestId:$requestId,userId:$userId");
      liveController.seatController.onRequestCancelled(requestId, userId);
    };

    super.onRequestProcessed = (requestId, userId) {
      LiveKitLogger.info("$tag onRequestProcessed:[requestId:$requestId,userId:$userId");
      liveController.seatController.onRequestProcessed(requestId, userId);
    };

    super.onKickedOffSeat = (seatIndex, userInfo) {
      LiveKitLogger.info("$tag onKickedOffSeat:[seatIndex:$seatIndex,userInfo:$userInfo");
      liveController.seatController.onKickedOffSeat(seatIndex, userInfo);
    };

    super.onUserAudioStateChanged = (userId, hasAudio, reason) {
      LiveKitLogger.info("$tag onUserAudioStateChanged:[userId:$userId,hasAudio:$hasAudio,reason:$reason");
      liveController.userController.onUserAudioStateChanged(userId, hasAudio, reason);
    };

    super.onUserVideoStateChanged = (userId, streamType, hasAudio, reason) {
      LiveKitLogger.info("$tag onUserVideoStateChanged:[userId:$userId,streamType:$streamType"
          ",hasAudio:$hasAudio,reason:$reason");
      liveController.userController.onUserVideoStateChanged(userId, streamType, hasAudio, reason);
    };

    super.onUserVoiceVolumeChanged = (volumeMap) {
      liveController.userController.onUserVoiceVolumeChanged(volumeMap);
    };

    super.onRemoteUserEnterRoom = (roomId, userInfo) {
      LiveKitLogger.info("$tag onRemoteUserEnterRoom:[roomId:$roomId,userInfo:$userInfo");
      liveController.userController.onRemoteUserEnterRoom(roomId, userInfo);
    };

    super.onRemoteUserLeaveRoom = (roomId, userInfo) {
      LiveKitLogger.info("$tag onRemoteUserLeaveRoom:[roomId:$roomId,userInfo:$userInfo");
      liveController.userController.onRemoteUserLeaveRoom(roomId, userInfo);
    };

    super.onKickedOffLine = (message) {
      LiveKitLogger.info("$tag onKickedOffLine:[message:$message");
      ErrorHandler.handleMessage(message);
      liveController.viewController.finish();
    };

    super.onKickedOutOfRoom = (roomId, reason, message) {
      LiveKitLogger.info("$tag onKickedOutOfRoom:[roomId:$roomId,reason:$reason,message:$message");
      if (reason != TUIKickedOutOfRoomReason.byLoggedOnOtherDevice) {
        ErrorHandler.handleMessage(message);
        liveController.viewController.finish();
      }
    };

  }
}