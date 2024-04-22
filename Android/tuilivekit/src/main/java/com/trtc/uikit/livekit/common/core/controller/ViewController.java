package com.trtc.uikit.livekit.common.core.controller;

import com.trtc.uikit.livekit.common.core.service.RoomEngineService;
import com.trtc.uikit.livekit.common.core.store.state.LiveState;
import com.trtc.uikit.livekit.common.core.store.state.view.ViewState;

public class ViewController extends Controller {

    public ViewController(LiveState state, RoomEngineService service) {
        super(state, service);
    }

    public void finish() {
        mViewState.currentNavigationState.set(ViewState.NavigationState.EXIT);
    }

    public void setShowAnchorPreview(boolean isShow) {
        mViewState.showAnchorPreview.set(isShow);
    }

    public void setShowEndView(boolean isShow) {
        mViewState.showEndView.set(isShow);
    }

    @Override
    public void destroy() {

    }
}
