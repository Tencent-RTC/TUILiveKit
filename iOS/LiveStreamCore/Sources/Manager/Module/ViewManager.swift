//
//  ViewManager.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/30.
//

import RTCCommon

class ViewManager {
    let observerState = ObservableState<ViewState>(initialState: ViewState())
    var viewState: ViewState {
        observerState.state
    }
    
    private weak var context: LiveStreamManager.Context?
    private let service: LiveStreamService
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        self.service = context.service
    }
    
    func getLocalLiveView() -> LiveStreamView {
        if let liveView = viewState.localLiveView {
            return liveView
        } else {
            let newView = LiveStreamView()
            modifyViewState(value: newView, keyPath: \ViewState.localLiveView)
            return newView
        }
    }
    
    func clearLocalLiveView() {
        modifyViewState(value: nil, keyPath: \ViewState.localLiveView)
    }
    
    func getRemoteLiveViewByUserId(userId: String) -> LiveStreamView {
        if let liveView = viewState.remoteLiveViewMap[userId] {
            return liveView
        } else {
            let newLiveView = LiveStreamView()
            observerState.update(isPublished: false) { viewState in
                viewState.remoteLiveViewMap[userId] = newLiveView
            }
            return newLiveView
        }
    }
    
    func removeRemoteView(userId: String) {
        observerState.update(isPublished: false) { viewState in
            viewState.remoteLiveViewMap.removeValue(forKey: userId)
        }
    }
    
    func setLayoutMode(layoutMode: LayoutMode, layoutJson: String? = nil) {
        var layoutConfig: LayoutConfig?
        
        if layoutMode == .freeLayout {
            layoutConfig = decodeLayoutConfig(from: layoutJson)
        } else {
            layoutConfig = loadLayoutConfig(for: layoutMode)
        }
        
        if let layoutConfig = layoutConfig {
            modifyViewState(value: layoutConfig, keyPath: \ViewState.layoutConfig)
            modifyViewState(value: layoutMode, keyPath: \ViewState.layoutMode, isPublished: true)
        }
    }
    
    func onLeaveRoom() {
        modifyViewState(value: nil, keyPath: \ViewState.localLiveView)
        modifyViewState(value: [:], keyPath: \ViewState.remoteLiveViewMap)
    }
}

// MARK: - Private
extension ViewManager {
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
    
    private func modifyViewState<T>(value: T, keyPath: WritableKeyPath<ViewState, T>, isPublished: Bool = false) {
        observerState.update(isPublished: isPublished) { viewState in
            viewState[keyPath: keyPath] = value
        }
    }
}
