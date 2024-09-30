package com.trtc.uikit.livekit.manager.controller;

import com.trtc.uikit.livekit.service.ILiveService;
import com.trtc.uikit.livekit.state.LiveState;
import com.trtc.uikit.livekit.state.operation.BattleState;
import com.trtc.uikit.livekit.state.operation.BeautyState;
import com.trtc.uikit.livekit.state.operation.ConnectionState;
import com.trtc.uikit.livekit.state.operation.MediaState;
import com.trtc.uikit.livekit.state.operation.RoomState;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.state.operation.UserState;
import com.trtc.uikit.livekit.state.view.ViewState;

public abstract class Controller {
    protected LiveState       mState;
    protected RoomState       mRoomState;
    protected SeatState       mSeatState;
    protected UserState       mUserState;
    protected MediaState      mMediaState;
    protected BeautyState     mBeautyState;
    protected ViewState       mViewState;
    protected ConnectionState mConnectionState;
    protected BattleState     mBattleState;
    protected ILiveService    mLiveService;

    protected Controller(LiveState state, ILiveService service) {
        mState = state;
        mRoomState = mState.operationState.roomState;
        mSeatState = mState.operationState.seatState;
        mUserState = mState.operationState.userState;
        mMediaState = mState.operationState.mediaState;
        mBeautyState = mState.operationState.beautyState;
        mConnectionState = mState.operationState.connectionState;
        mBattleState = mState.operationState.battleState;
        mViewState = mState.viewState;
        mLiveService = service;
    }

    protected abstract void destroy();
}
