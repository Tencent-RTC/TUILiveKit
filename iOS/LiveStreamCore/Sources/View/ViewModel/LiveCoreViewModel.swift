//
//  LiveCoreViewModel.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/12/20.
//

class LiveCoreViewModel {
    private var localLiveView: LiveStreamView? = nil
    private var remoteLiveViewMap: [String: LiveStreamView] = [:]
    private var layoutMode: LayoutMode = .gridLayout
    var layoutConfig: LayoutConfig? = nil
    
    func getLocalLiveView() -> LiveStreamView {
        if let liveView = localLiveView {
            return liveView
        } else {
            let newView = LiveStreamView()
            localLiveView = newView
            return newView
        }
    }
    
    func clearLocalLiveView() {
        localLiveView = nil
    }
    
    func getRemoteLiveViewByUserId(userId: String) -> LiveStreamView {
        if let liveView = remoteLiveViewMap[userId] {
            return liveView
        } else {
            let newLiveView = LiveStreamView()
            remoteLiveViewMap[userId] = newLiveView
            return newLiveView
        }
    }
    
    func removeRemoteView(userId: String) {
        remoteLiveViewMap.removeValue(forKey: userId)
    }
    
    func setLayoutMode(layoutMode: LayoutMode, layoutJson: String? = nil) {
        var layoutConfig: LayoutConfig?
        
        if layoutMode == .freeLayout {
            layoutConfig = decodeLayoutConfig(from: layoutJson)
        } else {
            layoutConfig = loadLayoutConfig(for: layoutMode)
        }
        
        if let layoutConfig = layoutConfig {
            self.layoutConfig = layoutConfig
            self.layoutMode = layoutMode
        }
    }
}

// MARK: - private
extension LiveCoreViewModel {
    private func decodeLayoutConfig(from jsonString: String?) -> LayoutConfig? {
        guard let jsonString = jsonString, let jsonData = jsonString.data(using: .utf8) else {
            debugPrint("Error: layoutJson is nil or cannot be converted to Data.")
            return nil
        }

        let decoder = JSONDecoder()
        do {
            return try decoder.decode(LayoutConfig.self, from: jsonData)
        } catch {
            debugPrint("Error decoding JSON: \(error)")
            return nil
        }
    }

    private func loadLayoutConfig(for layoutMode: LayoutMode) -> LayoutConfig? {
        let pathName = layoutMode == .gridLayout ? Constants.JsonName.gridLayout : Constants.JsonName.floatLayout
        guard let path = Bundle.liveStreamCoreBundle.path(forResource: pathName, ofType: "json") else {
            debugPrint("JSON file not found.")
            return nil
        }

        do {
            let data = try Data(contentsOf: URL(fileURLWithPath: path))
            let decoder = JSONDecoder()
            return try decoder.decode(LayoutConfig.self, from: data)
        } catch {
            debugPrint("Error loading or parsing JSON: \(error)")
            return nil
        }
    }
}
