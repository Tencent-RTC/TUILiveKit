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

    @Override
    public void destroy() {

    }

    public void enableAutoOpenCameraOnSeated(boolean enable) {
        mViewState.autoOpenCameraOnSeated.set(enable);
    }

    public void updateLiveStatus(LiveDefine.LiveStatus status) {
        mViewState.liveStatus.set(status);
    }

    public void onLiveEnd(String roomId) {
        ToastUtils.toast(R.string.livekit_room_destroy);
        updateLiveStatus(LiveDefine.LiveStatus.DASHBOARD);
    }
}
