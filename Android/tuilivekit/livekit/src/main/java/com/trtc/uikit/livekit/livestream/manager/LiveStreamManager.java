package com.trtc.uikit.livekit.livestream.manager;

import com.trtc.uikit.livekit.livestream.manager.api.ILiveService;
import com.trtc.uikit.livekit.livestream.manager.api.LiveStreamLog;
import com.trtc.uikit.livekit.livestream.manager.api.impl.LiveServiceImpl;
import com.trtc.uikit.livekit.livestream.manager.module.BattleManager;
import com.trtc.uikit.livekit.livestream.manager.module.CoGuestManager;
import com.trtc.uikit.livekit.livestream.manager.module.CoHostManager;
import com.trtc.uikit.livekit.livestream.manager.module.DashboardManager;
import com.trtc.uikit.livekit.livestream.manager.module.MediaManager;
import com.trtc.uikit.livekit.livestream.manager.module.RoomManager;
import com.trtc.uikit.livekit.livestream.manager.module.UserManager;
import com.trtc.uikit.livekit.livestream.manager.observer.LiveListManagerObserver;
import com.trtc.uikit.livekit.livestream.manager.observer.RoomEngineObserver;
import com.trtc.uikit.livekit.livestream.state.BattleState;
import com.trtc.uikit.livekit.livestream.state.BeautyState;
import com.trtc.uikit.livekit.livestream.state.CoGuestState;
import com.trtc.uikit.livekit.livestream.state.CoHostState;
import com.trtc.uikit.livekit.livestream.state.DashboardState;
import com.trtc.uikit.livekit.livestream.state.LiveState;
import com.trtc.uikit.livekit.livestream.state.MediaState;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestream.state.UserState;

public class LiveStreamManager {
    private final String                  mTag = "LiveStreamManager[" + hashCode() + "]";
    private final RoomManager             mRoomManager;
    private final CoGuestManager          mCoGuestManager;
    private final UserManager             mUserManager;
    private final MediaManager            mMediaManager;
    private final CoHostManager           mCoHostManager;
    private final BattleManager           mBattleManager;
    private final DashboardManager        mDashboardManager;
    private final LiveState               mState;
    private final ILiveService            mLiveService;
    private final RoomEngineObserver      mRoomEngineObserver;
    private final LiveListManagerObserver mLiveListManagerObserver;

    public LiveStreamManager() {
        mState = new LiveState();

        mLiveService = new LiveServiceImpl();
        mRoomManager = new RoomManager(mState, mLiveService);
        mCoGuestManager = new CoGuestManager(mState, mLiveService);
        mUserManager = new UserManager(mState, mLiveService);
        mMediaManager = new MediaManager(mState, mLiveService);
        mCoHostManager = new CoHostManager(mState, mLiveService);
        mBattleManager = new BattleManager(mState, mLiveService);
        mRoomEngineObserver = new RoomEngineObserver(this);
        mLiveListManagerObserver = new LiveListManagerObserver(this);
        mDashboardManager = new DashboardManager(mState, mLiveService);
    }

    public void addObserver() {
        mLiveService.addRoomEngineObserver(mRoomEngineObserver);
        mLiveService.addLiveListManagerObserver(mLiveListManagerObserver);
    }

    public void removeObserver() {
        mLiveService.removeRoomEngineObserver(mRoomEngineObserver);
        mLiveService.removeLiveListManagerObserver(mLiveListManagerObserver);
    }

    public void destroy() {
        removeObserver();
        mRoomManager.destroy();
        mCoGuestManager.destroy();
        mUserManager.destroy();
        mMediaManager.destroy();
        mCoHostManager.destroy();
        mBattleManager.destroy();
        mLiveService.destroy();
    }

    public ILiveService getLiveService() {
        return mLiveService;
    }

    public RoomManager getRoomManager() {
        return mRoomManager;
    }

    public CoGuestManager getCoGuestManager() {
        return mCoGuestManager;
    }

    public UserManager getUserManager() {
        return mUserManager;
    }

    public MediaManager getMediaManager() {
        return mMediaManager;
    }

    public CoHostManager getCoHostManager() {
        return mCoHostManager;
    }

    public BattleManager getBattleManager() {
        return mBattleManager;
    }

    public DashboardManager getDashboardManager() {
        return mDashboardManager;
    }

    public LiveState getState() {
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

    public DashboardState getDashboardState() {
        return mState.dashboardState;
    }

    public UserState getUserState() {
        return mState.userState;
    }

    public MediaState getMediaState() {
        return mState.mediaState;
    }

    public BeautyState getBeautyState() {
        return mState.beautyState;
    }

    public void setRoomId(String roomId) {
        getRoomState().roomId = roomId;
        LiveStreamLog.info(mTag + " setRoomId:[mRoomId=" + roomId + ",mLiveService:" + mLiveService.hashCode()
                + ",mLiveObserver:" + mRoomEngineObserver.hashCode() + "]");
    }
}
