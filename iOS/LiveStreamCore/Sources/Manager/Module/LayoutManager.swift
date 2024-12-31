//
//  LayoutManager.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/30.
//

import RTCCommon

class LayoutManager {
    let observerState = ObservableState<LayoutState>(initialState: LayoutState())
    var layoutState:LayoutState {
        observerState.state
    }
    
    private weak var context: LiveStreamManager.Context?
    
    init(context: LiveStreamManager.Context) {
        self.context = context
    }
    
    func updateVideoLayout(layout: VideoLayoutInfo?) {
        observerState.update { viewState in
            viewState.videoLayout = layout
        }
    }
}
