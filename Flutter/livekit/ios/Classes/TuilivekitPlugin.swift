import Flutter
import UIKit

public class TuilivekitPlugin: NSObject, FlutterPlugin {
  static let TAG = "TuilivekitPlugin"
  public static func register(with registrar: FlutterPluginRegistrar) {
    let channel = FlutterMethodChannel(name: "tuilivekit", binaryMessenger: registrar.messenger())
    let instance = TuilivekitPlugin()
    registrar.addMethodCallDelegate(instance, channel: channel)
  }

  public func handle(_ call: FlutterMethodCall, result: @escaping FlutterResult) {
    switch call.method {
    case "getPlatformVersion":
      result("iOS " + UIDevice.current.systemVersion)
      case "apiLog":
      apiLog(call,result)
    default:
      result(FlutterMethodNotImplemented)
    }
  }

  public func apiLog(_ call: FlutterMethodCall, _ result: @escaping FlutterResult) {
      guard let arguments = call.arguments as? [String: Any],
            let logString = arguments["logString"] as? String,
            let level = arguments["level"] as? Int else {
          result(0)
          return
      }

      switch level {
      case 0:
          LiveKitLog.info("\(#file)", "\(#line)","\(logString)")
      case 1:
          LiveKitLog.warn("\(#file)", "\(#line)","\(logString)")
      case 2:
          LiveKitLog.error("\(#file)", "\(#line)","\(logString)")
      default:
          LiveKitLog.info("\(#file)", "\(#line)","\(logString)")
      }

      result(0)
  }
}
