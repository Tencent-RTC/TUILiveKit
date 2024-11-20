package com.trtc.uikit.livekit.voiceroom.manager;

import com.trtc.uikit.livekit.voiceroom.api.IVoiceRoom;
import com.trtc.uikit.livekit.voiceroom.api.Logger;
import com.trtc.uikit.livekit.voiceroom.api.impl.VoiceRoomImpl;
import com.trtc.uikit.livekit.voiceroom.manager.module.MediaManager;
import com.trtc.uikit.livekit.voiceroom.manager.module.RoomManager;
import com.trtc.uikit.livekit.voiceroom.manager.module.SeatManager;
import com.trtc.uikit.livekit.voiceroom.manager.module.UserManager;
import com.trtc.uikit.livekit.voiceroom.manager.observer.LiveListManagerObserver;
import com.trtc.uikit.livekit.voiceroom.manager.observer.RoomEngineObserver;
import com.trtc.uikit.livekit.voiceroom.state.MediaState;
import com.trtc.uikit.livekit.voiceroom.state.RoomState;
import com.trtc.uikit.livekit.voiceroom.state.SeatState;
import com.trtc.uikit.livekit.voiceroom.state.UserState;
import com.trtc.uikit.livekit.voiceroom.state.VoiceRoomState;

public class VoiceRoomManager {
    private static final String FILE = "VoiceRoomManager";

    private final RoomManager    mRoomManager;
    private final SeatManager    mSeatManager;
    private final UserManager    mUserManager;
    private final MediaManager   mMediaManager;
    private final VoiceRoomState mState;
    private final IVoiceRoom     mVoiceRoom;

    private final RoomEngineObserver      mRoomEngineObserver;
    private final LiveListManagerObserver mLiveListManagerObserver;

    public VoiceRoomManager() {
        mState = new VoiceRoomState();
        mVoiceRoom = new VoiceRoomImpl();
        mRoomManager = new RoomManager(mState, mVoiceRoom);
        mSeatManager = new SeatManager(mState, mVoiceRoom);
        mUserManager = new UserManager(mState, mVoiceRoom);
        mMediaManager = new MediaManager(mState, mVoiceRoom);
        mRoomEngineObserver = new RoomEngineObserver(this);
        mLiveListManagerObserver = new LiveListManagerObserver(this);
        mVoiceRoom.addRoomEngineObserver(mRoomEngineObserver);
        mVoiceRoom.addLiveListManagerObserver(mLiveListManagerObserver);
    }

    public void destroy() {
        destroyWithoutLiveService();
        mVoiceRoom.destroy();
    }

    public void destroyWithoutLiveService() {
        mVoiceRoom.removeRoomEngineObserver(mRoomEngineObserver);
        mVoiceRoom.removeLiveListManagerObserver(mLiveListManagerObserver);
        mRoomManager.destroy();
        mSeatManager.destroy();
        mUserManager.destroy();
        mMediaManager.destroy();
    }

    public RoomManager getRoomManager() {
        return mRoomManager;
    }

    public SeatManager getSeatManager() {
        return mSeatManager;
    }

    public UserManager getUserManager() {
        return mUserManager;
    }

    public MediaManager getMediaManager() {
        return mMediaManager;
    }

    public VoiceRoomState getState() {
        return mState;
    }

    public RoomState getRoomState() {
        return mState.roomState;
    }

    public SeatState getSeatState() {
        return mState.seatState;
    }

    public UserState getUserState() {
        return mState.userState;
    }

    public MediaState getMediaState() {
        return mState.mediaState;
    }

    public void setRoomId(String roomId) {
        getRoomState().roomId = roomId;
        Logger.info(FILE, " setRoomId:[mRoomId=" + roomId + ",mLiveService:" + ",mLiveObserver:"
                + mRoomEngineObserver.hashCode() + "]");
    }
}
