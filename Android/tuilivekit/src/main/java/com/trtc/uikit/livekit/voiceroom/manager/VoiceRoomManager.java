package com.trtc.uikit.livekit.voiceroom.manager;

import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.voiceroom.manager.api.IVoiceRoom;
import com.trtc.uikit.livekit.voiceroom.manager.api.impl.VoiceRoomImpl;
import com.trtc.uikit.livekit.voiceroom.manager.module.MediaManager;
import com.trtc.uikit.livekit.voiceroom.manager.module.RoomManager;
import com.trtc.uikit.livekit.voiceroom.manager.module.SeatManager;
import com.trtc.uikit.livekit.voiceroom.manager.module.UserManager;
import com.trtc.uikit.livekit.voiceroom.manager.observer.IMFriendshipListener;
import com.trtc.uikit.livekit.voiceroom.manager.observer.LiveListManagerObserver;
import com.trtc.uikit.livekit.voiceroom.manager.observer.RoomEngineObserver;
import com.trtc.uikit.livekit.voiceroom.state.MediaState;
import com.trtc.uikit.livekit.voiceroom.state.RoomState;
import com.trtc.uikit.livekit.voiceroom.state.SeatState;
import com.trtc.uikit.livekit.voiceroom.state.UserState;
import com.trtc.uikit.livekit.voiceroom.state.VoiceRoomState;
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine.CoreState;

public class VoiceRoomManager {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getVoiceRoomLogger("VoiceRoomManager");

    private final RoomManager    mRoomManager;
    private final SeatManager    mSeatManager;
    private final UserManager    mUserManager;
    private final MediaManager   mMediaManager;
    private final VoiceRoomState mState;
    private final IVoiceRoom     mVoiceRoom;

    private final RoomEngineObserver      mRoomEngineObserver;
    private final LiveListManagerObserver mLiveListManagerObserver;
    private final IMFriendshipListener    mIMFriendshipListener;
    private       CoreStateProvider       mCoreStateProvider;

    public VoiceRoomManager() {
        mState = new VoiceRoomState();
        mVoiceRoom = new VoiceRoomImpl();
        mRoomManager = new RoomManager(mState, mVoiceRoom);
        mSeatManager = new SeatManager(mState, mVoiceRoom);
        mUserManager = new UserManager(mState, mVoiceRoom);
        mMediaManager = new MediaManager(mState, mVoiceRoom);
        mRoomEngineObserver = new RoomEngineObserver(this);
        mLiveListManagerObserver = new LiveListManagerObserver(this);
        mIMFriendshipListener = new IMFriendshipListener(this);
        mVoiceRoom.addRoomEngineObserver(mRoomEngineObserver);
        mVoiceRoom.addLiveListManagerObserver(mLiveListManagerObserver);
        mVoiceRoom.addFriendListener(mIMFriendshipListener);
    }

    public void destroy() {
        destroyWithoutLiveService();
        mVoiceRoom.destroy();
    }

    public void destroyWithoutLiveService() {
        mVoiceRoom.removeRoomEngineObserver(mRoomEngineObserver);
        mVoiceRoom.removeLiveListManagerObserver(mLiveListManagerObserver);
        mVoiceRoom.removeFriendListener(mIMFriendshipListener);
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
        LOGGER.info(hashCode() + " setRoomId:[mRoomId=" + roomId + ",mLiveService:" + ",mLiveObserver:"
                + mRoomEngineObserver.hashCode() + "]");
    }

    public CoreState getCoreState() {
        return mCoreStateProvider.getCoreState();
    }

    public void setCoreStateProvider(CoreStateProvider provider) {
        mCoreStateProvider = provider;
    }

    public interface CoreStateProvider {
        CoreState getCoreState();
    }
}
