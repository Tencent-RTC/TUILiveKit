import 'package:tencent_live_uikit/service/impl/live_service_impl.dart';
import 'package:tencent_live_uikit/service/live_service.dart';

class ServiceProvider {
  ServiceProvider._();

  static final ServiceProvider instance = ServiceProvider._();

  ILiveService getLiveService() {
    return LiveServiceImpl();
  }
}
