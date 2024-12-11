import 'package:barrage/manager/index.dart';
import 'package:barrage/state/index.dart';

class BarrageStore {
  static BarrageStore? _instance;

  BarrageStore._internal();

  factory BarrageStore() {
    _instance ??= BarrageStore._internal();
    return _instance!;
  }

  BarrageState state = BarrageState();

  BarrageManager manager = BarrageManager();

  String roomId = "";

  String selfUserId = "";

  String selfName = "";

  String ownerId = "";

  void init(String roomId, String ownerId, String userId, String? name) {
    if (this.roomId != roomId || this.ownerId != ownerId || selfUserId != userId ) {
      this.roomId = roomId;
      this.ownerId = ownerId;
      selfUserId = userId;
      selfName = name ?? selfUserId;
      state.reset();
    } else {
      selfName = name ?? selfUserId;
    }
  }
}
