import 'package:tencent_live_uikit/service/index.dart';
import 'package:tencent_live_uikit/state/index.dart';

abstract class Controller {
  final LiveState liveState;
  late RoomState roomState;
  late SeatState seatState;
  late UserState userState;
  late MediaState mediaState;
  late BeautyState beautyState;
  late ViewState viewState;
  final ILiveService liveService;

  Controller(this.liveState, this.liveService) {
    roomState = liveState.operationState.roomState;
    seatState = liveState.operationState.seatState;
    userState = liveState.operationState.userState;
    mediaState = liveState.operationState.mediaState;
    beautyState = liveState.operationState.beautyState;
    viewState = liveState.viewState;
  }

  void destroy();
}