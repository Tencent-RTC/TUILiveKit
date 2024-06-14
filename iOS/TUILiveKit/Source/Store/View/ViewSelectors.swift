//
//  ViewSelectors.swift
//  TUILiveKit
//
//  Created by aby on 2024/5/28.
//

import Foundation

enum ViewSelectors {
    static let getViewState = Selector<ViewState, ViewState> { state in
        return state
    }
    static let getLiveStatus = Selector(keyPath: \ViewState.liveStatus)
    static let getLinkStatus = Selector(keyPath: \ViewState.linkStatus)
    static let autoOpenCameraOnSeated = Selector(keyPath: \ViewState.autoOpenCameraOnSeated)
}
