import 'impl/live_service_impl.dart';
import 'live_service.dart';

class ServiceProvider {
  ServiceProvider._();

  static final ServiceProvider instance = ServiceProvider._();

  ILiveService getLiveService() {
    return LiveServiceImpl();
  }
}
