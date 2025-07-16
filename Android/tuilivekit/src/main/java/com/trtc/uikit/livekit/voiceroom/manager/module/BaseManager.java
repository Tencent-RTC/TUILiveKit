package com.trtc.uikit.livekit.voiceroom.manager.module;

import com.trtc.uikit.livekit.voiceroom.manager.api.IVoiceRoom;
import com.trtc.uikit.livekit.voiceroom.state.MediaState;
import com.trtc.uikit.livekit.voiceroom.state.RoomState;
import com.trtc.uikit.livekit.voiceroom.state.SeatState;
import com.trtc.uikit.livekit.voiceroom.state.UserState;
import com.trtc.uikit.livekit.voiceroom.state.VoiceRoomState;

public abstract class BaseManager {
    protected VoiceRoomState mState;
    protected RoomState      mRoomState;
    protected SeatState      mSeatState;
    protected UserState      mUserState;
    protected MediaState     mMediaState;
    protected IVoiceRoom     mLiveService;

    protected BaseManager(VoiceRoomState state, IVoiceRoom service) {
        mState = state;
        mRoomState = mState.roomState;
        mSeatState = mState.seatState;
        mUserState = mState.userState;
        mMediaState = mState.mediaState;
        mLiveService = service;
    }

    protected abstract void destroy();
}
