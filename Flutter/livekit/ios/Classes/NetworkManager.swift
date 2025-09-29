import Flutter
import UIKit
import Network

class NetworkManager:NSObject, FlutterStreamHandler {
    private let connected = "connected"
    private let disconnected = "disconnected"
    private var eventSink: FlutterEventSink?
    private let monitor = NWPathMonitor()
    private var currentPath: NWPath?
    private let queue = DispatchQueue(label: "com.network.monitor")
    
    public func getCurrentNetworkStatus() -> String {
        guard let path = currentPath else { return disconnected }
        let isConnected = path.status == .satisfied
        return isConnected ? connected : disconnected
    }

    func onListen(withArguments arguments: Any?, eventSink events: @escaping FlutterEventSink) -> FlutterError? {
        self.eventSink = events
        startNetworkMonitoring()
        return nil
    }
    
    func onCancel(withArguments arguments: Any?) -> FlutterError? {
        stopNetworkMonitoring()
        self.eventSink = nil
        return nil
    }
}

extension NetworkManager {
    private func startNetworkMonitoring() {
        monitor.pathUpdateHandler = { [weak self] path in
            guard let self = self else { return }
            self.currentPath = path
            let isConnected = path.status == .satisfied
            if let sink = eventSink {
                sink(isConnected ? connected : disconnected)
            }
            
        }
        monitor.start(queue: queue)
    }
    
    private func stopNetworkMonitoring() {
        monitor.cancel()
    }
}
