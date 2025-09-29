import Flutter
import UIKit

public class TuilivekitPlugin: NSObject, FlutterPlugin {
  static let TAG = "TuilivekitPlugin"
  var settingsManager: SettingsManager?
  let thermalManager = ThermalManager()
  let networkManager = NetworkManager()

  public static func register(with registrar: FlutterPluginRegistrar) {
    let channel = FlutterMethodChannel(name: "tuilivekit", binaryMessenger: registrar.messenger())
    let instance = TuilivekitPlugin()
    registrar.addMethodCallDelegate(instance, channel: channel)

    let thermalEventChannel = FlutterEventChannel(name: "tuilivekit_thermal_events", binaryMessenger: registrar.messenger())
    thermalEventChannel.setStreamHandler(instance.thermalManager)
    
    let networkEventChannel = FlutterEventChannel(name: "tuilivekit_network_events", binaryMessenger: registrar.messenger())
    networkEventChannel.setStreamHandler(instance.networkManager)
  }

  public func handle(_ call: FlutterMethodCall, result: @escaping FlutterResult) {
    switch call.method {
    case "getPlatformVersion":
      result("iOS " + UIDevice.current.systemVersion)
    case "apiLog":
      apiLog(call,result)
    case "enableWakeLock":
      enableWakeLock(call, result)
    case "openWifiSettings":
       openWifiSettings(call, result)
    case "openAppSettings":
       openAppSettings(call, result)
    case "getCurrentNetworkStatus":
        getCurrentNetworkStatus(call, result)
    default:
      result(FlutterMethodNotImplemented)
    }
  }

    public func apiLog(_ call: FlutterMethodCall, _ result: @escaping FlutterResult) {
      guard let arguments = call.arguments as? [String: Any],
        let logString = arguments["logString"] as? String,
        let level = arguments["level"] as? Int,
        let module = arguments["module"] as? String,
        let file = arguments["file"] as? String,
        let line = arguments["line"] as? Int
      else {
        result(0)
        return
      }

      switch level {
      case 0:
        LiveKitLog.info("\(module)", "\(file)", "\(line)", "\(logString)")
      case 1:
        LiveKitLog.warn("\(module)", "\(file)", "\(line)", "\(logString)")
      case 2:
        LiveKitLog.error("\(module)", "\(file)", "\(line)", "\(logString)")
      default:
        LiveKitLog.info("\(module)", "\(file)", "\(line)", "\(logString)")
      }

      result(0)
   }

    public func enableWakeLock(_ call: FlutterMethodCall, _ result: @escaping FlutterResult) {
      guard let arguments = call.arguments as? [String: Any],
        let enable = arguments["enable"] as? Bool
      else {
        result(0)
        return
      }
        
       DispatchQueue.main.async {
          UIApplication.shared.isIdleTimerDisabled = enable == true
          debugPrint("enableWakeLock: \(enable) isIdleTimerDisabled:\(UIApplication.shared.isIdleTimerDisabled)")
          result(0)
      }
    }

    public func openWifiSettings(_ call: FlutterMethodCall, _ result: @escaping FlutterResult) {
        settingsManager = SettingsManager(viewController: UIApplication.shared.windows.first?.rootViewController)
        settingsManager?.openWifiSettings()
        result(nil)
    }

    public func openAppSettings(_ call: FlutterMethodCall, _ result: @escaping FlutterResult) {
        settingsManager = SettingsManager(viewController: UIApplication.shared.windows.first?.rootViewController)
        settingsManager?.openAppSettings()
        result(nil)
    }
    
    public func getCurrentNetworkStatus(_ call: FlutterMethodCall, _ result: @escaping FlutterResult) {
        result(networkManager.getCurrentNetworkStatus())
    }
}
