package com.trtc.uikit.livekit.features.audiencecontainer.manager;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.features.audiencecontainer.AudienceContainerViewDefine.AudienceContainerViewListener;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.api.ILiveService;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.api.impl.LiveServiceImpl;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.module.BattleManager;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.module.CoGuestManager;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.module.MediaManager;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.module.RoomManager;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.module.UserManager;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.observer.AudienceContainerViewListenerList;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.observer.AudienceViewListenerList;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.observer.IMFriendshipListener;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.observer.LiveLayoutManagerObserver;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.observer.RoomEngineObserver;
import com.trtc.uikit.livekit.features.audiencecontainer.state.AudienceState;
import com.trtc.uikit.livekit.features.audiencecontainer.state.BattleState;
import com.trtc.uikit.livekit.features.audiencecontainer.state.CoGuestState;
import com.trtc.uikit.livekit.features.audiencecontainer.state.MediaState;
import com.trtc.uikit.livekit.features.audiencecontainer.state.RoomState;
import com.trtc.uikit.livekit.features.audiencecontainer.state.UserState;
import io.trtc.tuikit.atomicxcore.api.deprecated.LiveCoreViewDefine.CoreState;

public class AudienceManager {
    private       CoreState                         mCoreState;
    private final RoomManager                       mRoomManager;
    private final CoGuestManager                    mCoGuestManager;
    private final UserManager                       mUserManager;
    private final MediaManager                      mMediaManager;
    private final BattleManager                     mBattleManager;
    private final AudienceState                     mState;
    private final ILiveService                      mLiveService;
    private final RoomEngineObserver                mRoomEngineObserver;
    private final IMFriendshipListener              mIMFriendshipListener;
    private final LiveLayoutManagerObserver         mLiveLayoutManagerObserver;
    private final AudienceViewListenerList          mAudienceViewListenerList;
    private       AudienceContainerViewListenerList mAudienceContainerViewListenerList;

    public AudienceManager() {
        mState = new AudienceState();

        mLiveService = new LiveServiceImpl();
        mRoomManager = new RoomManager(mState, mLiveService);
        mCoGuestManager = new CoGuestManager(mState, mLiveService);
        mUserManager = new UserManager(mState, mLiveService);
        mMediaManager = new MediaManager(mState, mLiveService);
        mBattleManager = new BattleManager(mState, mLiveService);
        mRoomEngineObserver = new RoomEngineObserver(this);
        mIMFriendshipListener = new IMFriendshipListener(this);
        mAudienceViewListenerList = new AudienceViewListenerList();
        mLiveLayoutManagerObserver = new LiveLayoutManagerObserver(this);
    }

    public void setAudienceContainerViewListenerList(AudienceContainerViewListenerList viewListenerList) {
        mAudienceContainerViewListenerList = viewListenerList;
    }

    public void addObserver() {
        mLiveService.addRoomEngineObserver(mRoomEngineObserver);
        mLiveService.addFriendListener(mIMFriendshipListener);
        mLiveService.addLiveLayoutManagerObserver(mLiveLayoutManagerObserver);
    }

    public void removeObserver() {
        mLiveService.removeRoomEngineObserver(mRoomEngineObserver);
        mLiveService.removeFriendListener(mIMFriendshipListener);
        mLiveService.removeLiveLayoutManagerObserver(mLiveLayoutManagerObserver);
        if (mAudienceViewListenerList != null) {
            mAudienceViewListenerList.clearListeners();
        }
    }

    public void addAudienceViewListener(AudienceViewListener listener) {
        mAudienceViewListenerList.addListener(listener);
    }

    public void removeAudienceViewListener(AudienceViewListener listener) {
        mAudienceViewListenerList.removeListener(listener);
    }

    public void destroy() {
        removeObserver();
        mMediaManager.destroy();
        mBattleManager.destroy();
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

    public BattleManager getBattleManager() {
        return mBattleManager;
    }

    public AudienceState getState() {
        return mState;
    }

    public RoomState getRoomState() {
        return mState.roomState;
    }

    public CoGuestState getCoGuestState() {
        return mState.coGuestState;
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

    public CoreState getCoreState() {
        return mCoreState;
    }

    public void setCoreState(CoreState coreState) {
        mCoreState = coreState;
        mRoomManager.setCoreState(coreState);
        mMediaManager.setCoreState(coreState);
        mUserManager.setCoreState(coreState);
        mCoGuestManager.setCoreState(coreState);
        mBattleManager.setCoreState(coreState);
    }

    public void enablePictureInPictureMode(boolean enable) {
        mMediaManager.enablePictureInPictureMode(enable);
    }

    public void notifyOnRoomDismissed(String roomId) {
        if (mAudienceViewListenerList != null) {
            mAudienceViewListenerList.notifyListeners(listener -> listener.onRoomDismissed(roomId));
        }
        if (mAudienceContainerViewListenerList != null) {
            TUIRoomDefine.UserInfo ownerInfo = mCoreState.roomState.ownerInfo.getValue();
            String userName = ownerInfo == null ? "" : ownerInfo.userName;
            String avatarUrl = ownerInfo == null ? "" : ownerInfo.avatarUrl;
            mAudienceContainerViewListenerList.notifyListeners(
                    listener -> listener.onLiveEnded(roomId, userName, avatarUrl));
        }
    }

    public void notifyPictureInPictureClick() {
        if (mAudienceContainerViewListenerList != null) {
            mAudienceContainerViewListenerList.notifyListeners(AudienceContainerViewListener::onPictureInPictureClick);
        }
    }

    public interface AudienceViewListener {
        void onRoomDismissed(String roomId);
    }
}
