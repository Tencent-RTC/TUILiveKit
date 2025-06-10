package com.trtc.uikit.livekit.livestream.manager.module;

import com.trtc.uikit.livekit.livestream.manager.api.ILiveService;
import com.trtc.uikit.livekit.livestream.state.BattleState;
import com.trtc.uikit.livekit.livestream.state.CoGuestState;
import com.trtc.uikit.livekit.livestream.state.CoHostState;
import com.trtc.uikit.livekit.livestream.state.DashboardState;
import com.trtc.uikit.livekit.livestream.state.LiveState;
import com.trtc.uikit.livekit.livestream.state.MediaState;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestream.state.UserState;

public abstract class BaseManager {
    protected LiveState      mState;
    protected RoomState      mRoomState;
    protected CoGuestState   mCoGuestState;
    protected CoHostState    mCoHostState;
    protected UserState      mUserState;
    protected MediaState       mMediaState;
    protected BattleState      mBattleState;
    protected DashboardState mDashboardState;
    protected ILiveService   mLiveService;

    protected BaseManager(LiveState state, ILiveService service) {
        mState = state;
        mRoomState = mState.roomState;
        mCoGuestState = mState.coGuestState;
        mUserState = mState.userState;
        mMediaState = mState.mediaState;
        mCoHostState = mState.coHostState;
        mBattleState = mState.battleState;
        mDashboardState = mState.dashboardState;
        mLiveService = service;
    }

    protected abstract void destroy();
}
