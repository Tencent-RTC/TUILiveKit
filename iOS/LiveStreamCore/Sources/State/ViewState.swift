//
//  ViewState.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/22.
//

struct ViewState {
    var localLiveView: LiveStreamView? = nil
    var remoteLiveViewMap: [String: LiveStreamView] = [:]
    var layoutMode: LayoutMode = .gridLayout
    var layoutConfig: LayoutConfig? = nil
}
