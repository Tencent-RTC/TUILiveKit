package com.trtc.uikit.livekit.features.anchorboardcast.manager.module;

import com.trtc.uikit.livekit.features.anchorboardcast.manager.api.IAnchorAPI;
import com.trtc.uikit.livekit.features.anchorboardcast.state.AnchorState;
import com.trtc.uikit.livekit.features.anchorboardcast.state.BattleState;
import com.trtc.uikit.livekit.features.anchorboardcast.state.CoGuestState;
import com.trtc.uikit.livekit.features.anchorboardcast.state.CoHostState;
import com.trtc.uikit.livekit.features.anchorboardcast.state.MediaState;
import com.trtc.uikit.livekit.features.anchorboardcast.state.RoomState;
import com.trtc.uikit.livekit.features.anchorboardcast.state.UserState;
import io.trtc.tuikit.atomicxcore.api.deprecated.LiveCoreViewDefine.CoreState;

public abstract class BaseManager {
    protected AnchorState  mState;
    protected CoreState    mCoreState;
    protected RoomState    mRoomState;
    protected CoGuestState mCoGuestState;
    protected CoHostState  mCoHostState;
    protected UserState    mUserState;
    protected MediaState   mMediaState;
    protected BattleState  mBattleState;
    protected IAnchorAPI   mLiveService;

    protected BaseManager(AnchorState state, IAnchorAPI service) {
        mState = state;
        mRoomState = mState.roomState;
        mCoGuestState = mState.coGuestState;
        mUserState = mState.userState;
        mMediaState = mState.mediaState;
        mCoHostState = mState.coHostState;
        mBattleState = mState.battleState;
        mLiveService = service;
    }

    public void setCoreState(CoreState coreState) {
        mCoreState = coreState;
    }

    protected abstract void destroy();
}
