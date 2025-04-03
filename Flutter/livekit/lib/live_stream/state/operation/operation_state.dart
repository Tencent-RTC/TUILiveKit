import 'index.dart';

class OperationState {
  final RoomState roomState = RoomState();
  final SeatState seatState = SeatState();
  final UserState userState = UserState();
  final MediaState mediaState = MediaState();
  final BeautyState beautyState = BeautyState();

  void reset() {
    roomState.reset();
    seatState.reset();
    userState.reset();
    mediaState.reset();
    beautyState.reset();
  }
}
