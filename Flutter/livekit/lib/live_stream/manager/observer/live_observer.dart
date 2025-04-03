import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:rtc_room_engine/api/room/tui_room_observer.dart';

import '../../../common/index.dart';
import '../index.dart';

class LiveObserver extends TUIRoomObserver {
  static const String tag = "LiveObserver";
  WeakReference<LiveController> liveController;

  LiveObserver(this.liveController) {
    super.onRoomDismissed = (roomId, reason) {
      LiveKitLogger.info("$tag($hashCode) onRoomDismissed:[roomId:$roomId]");
      liveController.target?.viewController.onLiveEnd(roomId);
    };

    super.onRoomUserCountChanged = (roomId, userCount) {
      LiveKitLogger.info("$tag($hashCode)  onRoomUserCountChanged:[roomId:$roomId,userCount:$userCount]");
      liveController.target?.roomController.onRoomUserCountChanged(roomId, userCount);
    };

    super.onSeatListChanged = (seatList, seatedList, leftList) {
      LiveKitLogger.info(
          "$tag($hashCode) onSeatListChanged:[seatList:$seatList ,seatedList:$seatedList,leftList:$leftList]");
      liveController.target?.seatController.onSeatListChanged(seatList, seatedList, leftList);
    };

    super.onRequestReceived = (request) {
      LiveKitLogger.info("$tag($hashCode) onRequestReceived:[request:$request");
      liveController.target?.seatController.onRequestReceived(request);
    };

    super.onRequestCancelled = (requestId, userId) {
      LiveKitLogger.info("$tag($hashCode) onRequestCancelled:[requestId:$requestId,userId:$userId");
      liveController.target?.seatController.onRequestCancelled(requestId, userId);
    };

    super.onRequestProcessed = (requestId, userId) {
      LiveKitLogger.info("$tag($hashCode) onRequestProcessed:[requestId:$requestId,userId:$userId");
      liveController.target?.seatController.onRequestProcessed(requestId, userId);
    };

    super.onKickedOffSeat = (seatIndex, userInfo) {
      LiveKitLogger.info("$tag($hashCode) onKickedOffSeat:[seatIndex:$seatIndex,userInfo:$userInfo");
      liveController.target?.seatController.onKickedOffSeat(seatIndex, userInfo);
    };

    super.onUserAudioStateChanged = (userId, hasAudio, reason) {
      LiveKitLogger.info("$tag($hashCode) onUserAudioStateChanged:[userId:$userId,hasAudio:$hasAudio,reason:$reason");
      liveController.target?.userController.onUserAudioStateChanged(userId, hasAudio, reason);
    };

    super.onUserVideoStateChanged = (userId, streamType, hasAudio, reason) {
      LiveKitLogger.info("$tag($hashCode) onUserVideoStateChanged:[userId:$userId,streamType:$streamType"
          ",hasAudio:$hasAudio,reason:$reason");
      liveController.target?.userController.onUserVideoStateChanged(userId, streamType, hasAudio, reason);
    };

    super.onUserVoiceVolumeChanged = (volumeMap) {
      liveController.target?.userController.onUserVoiceVolumeChanged(volumeMap);
    };

    super.onRemoteUserEnterRoom = (roomId, userInfo) {
      LiveKitLogger.info("$tag($hashCode) onRemoteUserEnterRoom:[roomId:$roomId,userInfo:$userInfo");
      liveController.target?.userController.onRemoteUserEnterRoom(roomId, userInfo);
    };

    super.onRemoteUserLeaveRoom = (roomId, userInfo) {
      LiveKitLogger.info("$tag($hashCode) onRemoteUserLeaveRoom:[roomId:$roomId,userInfo:$userInfo");
      liveController.target?.userController.onRemoteUserLeaveRoom(roomId, userInfo);
    };

    super.onKickedOffLine = (message) {
      LiveKitLogger.info("$tag($hashCode) onKickedOffLine:[message:$message");
      ErrorHandler.handleMessage(message);
      liveController.target?.viewController.finish();
    };

    super.onKickedOutOfRoom = (roomId, reason, message) {
      LiveKitLogger.info("$tag($hashCode) onKickedOutOfRoom:[roomId:$roomId,reason:$reason,message:$message");
      if (reason != TUIKickedOutOfRoomReason.byLoggedOnOtherDevice) {
        ErrorHandler.handleMessage(message);
        liveController.target?.viewController.finish();
      }
    };
  }
}
