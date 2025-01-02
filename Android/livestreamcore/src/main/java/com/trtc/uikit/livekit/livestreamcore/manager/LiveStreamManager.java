package com.trtc.uikit.livekit.livestreamcore.manager;

import com.tencent.imsdk.v2.V2TIMManager;
import com.trtc.uikit.livekit.livestreamcore.manager.api.ILiveStream;
import com.trtc.uikit.livekit.livestreamcore.manager.api.impl.LiveStreamImpl;
import com.trtc.uikit.livekit.livestreamcore.manager.module.BattleManager;
import com.trtc.uikit.livekit.livestreamcore.manager.module.CoGuestManager;
import com.trtc.uikit.livekit.livestreamcore.manager.module.CoHostManager;
import com.trtc.uikit.livekit.livestreamcore.manager.module.MediaManager;
import com.trtc.uikit.livekit.livestreamcore.manager.module.RoomManager;
import com.trtc.uikit.livekit.livestreamcore.manager.module.UserManager;
import com.trtc.uikit.livekit.livestreamcore.manager.module.ViewManager;
import com.trtc.uikit.livekit.livestreamcore.manager.observer.IMObserver;
import com.trtc.uikit.livekit.livestreamcore.manager.observer.LiveBattleManagerObserver;
import com.trtc.uikit.livekit.livestreamcore.manager.observer.LiveConnectionManagerObserver;
import com.trtc.uikit.livekit.livestreamcore.manager.observer.LiveLayoutManagerObserver;
import com.trtc.uikit.livekit.livestreamcore.manager.observer.RoomEngineObserver;
import com.trtc.uikit.livekit.livestreamcore.state.BattleState;
import com.trtc.uikit.livekit.livestreamcore.state.CoGuestState;
import com.trtc.uikit.livekit.livestreamcore.state.CoHostState;
import com.trtc.uikit.livekit.livestreamcore.state.LiveStreamState;
import com.trtc.uikit.livekit.livestreamcore.state.MediaState;
import com.trtc.uikit.livekit.livestreamcore.state.RoomState;
import com.trtc.uikit.livekit.livestreamcore.state.UserState;
import com.trtc.uikit.livekit.livestreamcore.state.ViewState;

public class LiveStreamManager {
    private final RoomManager                   mRoomManager;
    private final CoGuestManager                mCoGuestManager;
    private final CoHostManager                 mCoHostManager;
    private final BattleManager                 mBattleManager;
    private final UserManager                   mUserManager;
    private final MediaManager                  mMediaManager;
    private final ViewManager                   mViewManager;
    private final LiveStreamState               mState;
    private final ILiveStream                   mLiveService;
    private final RoomEngineObserver            mRoomEngineObserver;
    private final LiveConnectionManagerObserver mliveConnectionManagerObserver;
    private final LiveBattleManagerObserver     mLiveBattleManagerObserver;
    private final LiveLayoutManagerObserver     mLiveLayoutManagerObserver;
    private final IMObserver                    mIMObserver;

    public LiveStreamManager() {
        mState = new LiveStreamState();

        mLiveService = new LiveStreamImpl();
        mRoomManager = new RoomManager(mState, mLiveService);
        mCoGuestManager = new CoGuestManager(mState, mLiveService);
        mUserManager = new UserManager(mState, mLiveService);
        mMediaManager = new MediaManager(mState, mLiveService);
        mCoHostManager = new CoHostManager(mState, mLiveService);
        mBattleManager = new BattleManager(mState, mLiveService);
        mViewManager = new ViewManager(mState, mLiveService);
        mRoomEngineObserver = new RoomEngineObserver(this);
        mliveConnectionManagerObserver = new LiveConnectionManagerObserver(this);
        mLiveBattleManagerObserver = new LiveBattleManagerObserver(this);
        mLiveLayoutManagerObserver = new LiveLayoutManagerObserver(this);
        mIMObserver = new IMObserver(this);
        mLiveService.addRoomEngineObserver(mRoomEngineObserver);
        mLiveService.addLiveConnectionManagerObserver(mliveConnectionManagerObserver);
        mLiveService.addLiveBattleManagerObserver(mLiveBattleManagerObserver);
        mLiveService.addLiveLayoutManagerObserver(mLiveLayoutManagerObserver);
        V2TIMManager.getInstance().addIMSDKListener(mIMObserver);
    }

    public void destroy() {
        destroyWithoutLiveService();
        mLiveService.destroy();
    }

    public void destroyWithoutLiveService() {
        mLiveService.removeRoomEngineObserver(mRoomEngineObserver);
        mLiveService.removeLiveConnectionManagerObserver(mliveConnectionManagerObserver);
        mLiveService.removeLiveBattleManagerObserver(mLiveBattleManagerObserver);
        mLiveService.removeLiveLayoutManagerObserver(mLiveLayoutManagerObserver);
        V2TIMManager.getInstance().removeIMSDKListener(mIMObserver);
        mRoomManager.destroy();
        mCoGuestManager.destroy();
        mUserManager.destroy();
        mMediaManager.destroy();
        mCoHostManager.destroy();
        mBattleManager.destroy();
        mViewManager.destroy();
    }

    public RoomManager getRoomManager() {
        return mRoomManager;
    }

    public CoGuestManager getCoGuestManager() {
        return mCoGuestManager;
    }

    public CoHostManager getCoHostManager() {
        return mCoHostManager;
    }

    public BattleManager getBattleManager() {
        return mBattleManager;
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

    public LiveStreamState getState() {
        return mState;
    }

    public RoomState getRoomState() {
        return mState.roomState;
    }

    public CoGuestState getCoGuestState() {
        return mState.coGuestState;
    }

    public CoHostState getCoHostState() {
        return mState.coHostState;
    }

    public BattleState getBattleState() {
        return mState.battleState;
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
}
