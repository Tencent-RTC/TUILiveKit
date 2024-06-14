package com.trtc.uikit.livekit.manager.controller;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.utils.ToastUtils;
import com.trtc.uikit.livekit.service.ILiveService;
import com.trtc.uikit.livekit.state.LiveDefine;
import com.trtc.uikit.livekit.state.LiveState;

public class ViewController extends Controller {

    public ViewController(LiveState state, ILiveService service) {
        super(state, service);
    }

    public void finish() {
        mViewState.currentNavigationState.set(LiveDefine.NavigationStatus.EXIT);
    }

    public void enableCamera(boolean enable) {
        mViewState.enableCamera.set(enable);
    }

    @Override
    public void destroy() {

    }

    public void onLiveEnd(String roomId) {
        ToastUtils.toast(R.string.livekit_room_destroy);
        mViewState.liveStatus.set(LiveDefine.LiveStatus.DASHBOARD);
    }
}
