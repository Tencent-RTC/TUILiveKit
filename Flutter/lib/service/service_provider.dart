import 'package:tencent_live_uikit/service/impl/room_engine_service.dart';
import 'package:tencent_live_uikit/service/live_service.dart';

class ServiceProvider {
  ServiceProvider._();

  static final ServiceProvider instance = ServiceProvider._();

  ILiveService getLiveService() {
    return RoomEngineService();
  }
}