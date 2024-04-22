package com.trtc.uikit.livekit.common.core.controller;

import com.trtc.uikit.livekit.common.core.service.RoomEngineService;
import com.trtc.uikit.livekit.common.core.store.state.LiveState;
import com.trtc.uikit.livekit.common.core.store.state.operation.MediaState;
import com.trtc.uikit.livekit.common.core.store.state.operation.RoomState;
import com.trtc.uikit.livekit.common.core.store.state.operation.SeatState;
import com.trtc.uikit.livekit.common.core.store.state.operation.UserState;
import com.trtc.uikit.livekit.common.core.store.state.view.ViewState;

public abstract class Controller {
    protected LiveState         mState;
    protected RoomState         mRoomState;
    protected SeatState         mSeatState;
    protected UserState         mUserState;
    protected MediaState        mMediaState;
    protected ViewState         mViewState;
    protected RoomEngineService mRoomEngineService;

    protected Controller(LiveState state, RoomEngineService service) {
        mState = state;
        mRoomState = mState.operationState.roomState;
        mSeatState = mState.operationState.seatState;
        mUserState = mState.operationState.userState;
        mMediaState = mState.operationState.mediaState;
        mViewState = mState.viewState;
        mRoomEngineService = service;
    }

    protected abstract void destroy();
}
