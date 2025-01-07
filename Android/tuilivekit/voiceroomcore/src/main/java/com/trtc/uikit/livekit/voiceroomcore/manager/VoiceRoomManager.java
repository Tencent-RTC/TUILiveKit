package com.trtc.uikit.livekit.voiceroomcore.manager;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.voiceroomcore.manager.module.MediaManager;
import com.trtc.uikit.livekit.voiceroomcore.manager.module.RoomManager;
import com.trtc.uikit.livekit.voiceroomcore.manager.module.SeatManager;
import com.trtc.uikit.livekit.voiceroomcore.manager.module.UserManager;
import com.trtc.uikit.livekit.voiceroomcore.manager.module.ViewManager;
import com.trtc.uikit.livekit.voiceroomcore.manager.observer.RoomEngineObserver;
import com.trtc.uikit.livekit.voiceroomcore.manager.observer.SeatGridViewObserverManager;
import com.trtc.uikit.livekit.voiceroomcore.manager.api.IVoiceRoomService;
import com.trtc.uikit.livekit.voiceroomcore.manager.api.impl.VoiceRoomServiceImpl;
import com.trtc.uikit.livekit.voiceroomcore.state.MediaState;
import com.trtc.uikit.livekit.voiceroomcore.state.RoomState;
import com.trtc.uikit.livekit.voiceroomcore.state.SeatState;
import com.trtc.uikit.livekit.voiceroomcore.state.UserState;
import com.trtc.uikit.livekit.voiceroomcore.state.ViewState;
import com.trtc.uikit.livekit.voiceroomcore.state.VoiceRoomState;

public class VoiceRoomManager {
    private final VoiceRoomState mState;

    private final RoomManager  mRoomManager;
    private final SeatManager  mSeatManager;
    private final UserManager  mUserManager;
    private final MediaManager mMediaManager;
    private final ViewManager  mViewManager;

    private final IVoiceRoomService  mService;
    private final RoomEngineObserver mRoomEngineObserver;

    public VoiceRoomManager(SeatGridViewObserverManager observerManager) {
        mState = new VoiceRoomState();
        mService = new VoiceRoomServiceImpl();

        mRoomManager = new RoomManager(mState, mService, observerManager);
        mSeatManager = new SeatManager(mState, mService, observerManager);
        mUserManager = new UserManager(mState, mService, observerManager);
        mViewManager = new ViewManager(mState, mService, observerManager);
        mMediaManager = new MediaManager(mState, mService, observerManager);
        mRoomEngineObserver = new RoomEngineObserver(this);
    }

    public void destroy() {
        mService.removeRoomEngineObserver(mRoomEngineObserver);
        mRoomManager.removeListener(mRoomManagerListener);
        mRoomManager.destroy();
        mUserManager.destroy();
        mMediaManager.destroy();
        mViewManager.destroy();
        mState.reset();
    }

    public void addObserver() {
        mService.addRoomEngineObserver(mRoomEngineObserver);
        mRoomManager.addListener(mRoomManagerListener);
    }

    public void removeObserver() {
        mService.removeRoomEngineObserver(mRoomEngineObserver);
        mRoomManager.removeListener(mRoomManagerListener);
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

    public ViewManager getViewManager() {
        return mViewManager;
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

    public ViewState getViewState() {
        return mState.viewState;
    }

    private void initSeatListWhenEnterRoom() {
        mViewManager.setLayoutMode(mState.viewState.layoutMode.get(), mState.viewState.layoutConfig.get());
        mSeatManager.setSeatList(mState.viewState.layoutConfig.get(), mState.roomState.maxSeatCount.get());
    }

    private final RoomManager.Listener mRoomManagerListener = new RoomManager.Listener() {
        @Override
        public void onEnterRoomSuccess() {
            mUserManager.updateOwnerUserInfo();
            initSeatListWhenEnterRoom();
            mSeatManager.getSeatList();
            if (mState.userState.selfInfo.userRole == TUIRoomDefine.Role.ROOM_OWNER) {
                mSeatManager.getSeatApplicationList();
            }
        }
    };
}
