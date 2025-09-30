package com.trtc.uikit.livekit.features.audiencecontainer.manager.module;

import com.trtc.uikit.livekit.features.audiencecontainer.manager.api.ILiveService;
import com.trtc.uikit.livekit.features.audiencecontainer.state.AudienceState;
import com.trtc.uikit.livekit.features.audiencecontainer.state.BattleState;
import com.trtc.uikit.livekit.features.audiencecontainer.state.CoGuestState;
import com.trtc.uikit.livekit.features.audiencecontainer.state.MediaState;
import com.trtc.uikit.livekit.features.audiencecontainer.state.RoomState;
import com.trtc.uikit.livekit.features.audiencecontainer.state.UserState;
import io.trtc.tuikit.atomicxcore.api.deprecated.LiveCoreViewDefine;
import io.trtc.tuikit.atomicxcore.api.deprecated.LiveCoreViewDefine.CoreState;

public abstract class BaseManager {
    protected AudienceState mState;
    protected CoreState     mCoreState;
    protected RoomState     mRoomState;
    protected CoGuestState  mCoGuestState;
    protected UserState     mUserState;
    protected MediaState    mMediaState;
    protected BattleState   mBattleState;
    protected ILiveService  mLiveService;

    protected BaseManager(AudienceState state, ILiveService service) {
        mState = state;
        mRoomState = mState.roomState;
        mCoGuestState = mState.coGuestState;
        mUserState = mState.userState;
        mMediaState = mState.mediaState;
        mBattleState = mState.battleState;
        mLiveService = service;
    }

    public void setCoreState(LiveCoreViewDefine.CoreState coreState) {
        mCoreState = coreState;
    }

    protected void destroy() {

    }
}
