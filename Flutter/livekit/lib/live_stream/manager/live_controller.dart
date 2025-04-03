import 'package:rtc_room_engine/api/common/tui_common_define.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';

import '../service/index.dart';
import '../state/index.dart';
import '../widget/live_room/video/video_widget_factory.dart';
import 'index.dart';

class LiveController {
  late RoomController roomController;
  late SeatController seatController;
  late UserController userController;
  late MediaController mediaController;
  late ViewController viewController;
  late LiveState state;
  late ILiveService liveService;
  late LiveObserver liveObserver;
  late VideoWidgetFactory videoWidgetFactory;

  LiveController() {
    state = LiveState();
    liveService = ServiceProvider.instance.getLiveService();
    roomController = RoomController(state, liveService);
    seatController = SeatController(state, liveService);
    userController = UserController(state, liveService);
    mediaController = MediaController(state, liveService);
    viewController = ViewController(state, liveService);
    videoWidgetFactory = VideoWidgetFactory(liveController: WeakReference(this));
    liveObserver = LiveObserver(WeakReference(this));
    liveService.addObserver(liveObserver);
  }

  void destroy() {
    liveService.removeObserver(liveObserver);
    roomController.destroy();
    seatController.destroy();
    userController.destroy();
    mediaController.destroy();
    viewController.destroy();
    liveService.destroy();
    videoWidgetFactory.clear();
  }

  void setRoomId(String roomId) {
    state.operationState.roomState.roomId = roomId;
  }

  RoomState getRoomSate() {
    return state.operationState.roomState;
  }

  SeatState getSeatState() {
    return state.operationState.seatState;
  }

  UserState getUserState() {
    return state.operationState.userState;
  }

  MediaState getMediaState() {
    return state.operationState.mediaState;
  }

  BeautyState getBeautyState() {
    return state.operationState.beautyState;
  }

  ViewState getViewState() {
    return state.viewState;
  }

  VideoWidgetFactory getVideoViewFactory() {
    return videoWidgetFactory;
  }

  bool isOwner() {
    if (getUserState().selfInfo.userId.isEmpty) {
      return false;
    }
    return getRoomSate().ownerInfo.userId == getUserState().selfInfo.userId;
  }

  void start() async {
    final startResult = await roomController.start();
    if (TUIError.success == startResult.code) {
      await userController.getAudienceList();
      await userController.updateOwnerUserInfo();
      await seatController.getSeatList();
      if (state.operationState.userState.selfInfo.role.value == TUIRole.roomOwner) {
        await seatController.getSeatApplicationList();
      }
    }
  }

  void join() async {
    final startResult = await roomController.join(getRoomSate().roomId);
    if (TUIError.success == startResult.code) {
      await userController.getAudienceList();
      await userController.updateOwnerUserInfo();
      await seatController.getSeatList();
      if (getRoomSate().ownerInfo.userId != getUserState().selfInfo.userId) {
        userController.checkFollowType(getRoomSate().ownerInfo.userId);
      }
    }
  }

  void exit() {
    if (isOwner()) {
      _stop();
    } else {
      _leave();
    }
  }

  void _stop() async {
    liveService.stop();
  }

  void _leave() async {
    liveService.leave();
  }
}
