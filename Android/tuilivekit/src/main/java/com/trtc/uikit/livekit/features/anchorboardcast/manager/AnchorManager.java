package com.trtc.uikit.livekit.features.anchorboardcast.manager;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.AnchorViewDefine;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.api.IAnchorAPI;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.api.impl.AnchorAPIImpl;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.module.AnchorViewListenerManager;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.module.BattleManager;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.module.CoGuestManager;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.module.CoHostManager;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.module.MediaManager;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.module.RoomManager;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.module.UserManager;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.observer.IMFriendshipListener;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.observer.LiveListManagerObserver;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.observer.RoomEngineObserver;
import com.trtc.uikit.livekit.features.anchorboardcast.state.AnchorConfig;
import com.trtc.uikit.livekit.features.anchorboardcast.state.AnchorState;
import com.trtc.uikit.livekit.features.anchorboardcast.state.BattleState;
import com.trtc.uikit.livekit.features.anchorboardcast.state.CoGuestState;
import com.trtc.uikit.livekit.features.anchorboardcast.state.CoHostState;
import com.trtc.uikit.livekit.features.anchorboardcast.state.MediaState;
import com.trtc.uikit.livekit.features.anchorboardcast.state.RoomState;
import com.trtc.uikit.livekit.features.anchorboardcast.state.UserState;
import io.trtc.tuikit.atomicxcore.api.deprecated.LiveCoreViewDefine.CoreState;

public class AnchorManager {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("AnchorManager");

    private final RoomManager                  mRoomManager;
    private final CoGuestManager               mCoGuestManager;
    private final UserManager                  mUserManager;
    private final MediaManager                 mMediaManager;
    private final CoHostManager                mCoHostManager;
    private final BattleManager                mBattleManager;
    private final AnchorViewListenerManager    mListenerManager;
    private final AnchorState                  mState;
    private final IAnchorAPI                   mLiveService;
    private final RoomEngineObserver           mRoomEngineObserver;
    private final LiveListManagerObserver      mLiveListManagerObserver;
    private final IMFriendshipListener         mIMFriendshipListener;
    private       CoreState                    mCoreState;
    private final AnchorViewDefine.AnchorState mExternalState;
    private       LiveStateListener            mLiveStateListener;

    public AnchorManager(TUILiveListManager.LiveInfo liveInfo) {
        mState = new AnchorState();

        mLiveService = new AnchorAPIImpl();
        mRoomManager = new RoomManager(mState, mLiveService);
        mCoGuestManager = new CoGuestManager(mState, mLiveService);
        mUserManager = new UserManager(mState, mLiveService);
        mMediaManager = new MediaManager(mState, mLiveService);
        mCoHostManager = new CoHostManager(mState, mLiveService);
        mBattleManager = new BattleManager(mState, mLiveService);
        mRoomEngineObserver = new RoomEngineObserver(this);
        mLiveListManagerObserver = new LiveListManagerObserver(this);
        mIMFriendshipListener = new IMFriendshipListener(this);
        mListenerManager = new AnchorViewListenerManager();

        addObserver();
        setRoomId(liveInfo.roomId);
        mMediaManager.setCustomVideoProcess();
        mMediaManager.enableMultiPlaybackQuality(true);
        mRoomManager.initCreateRoomState(liveInfo);

        mExternalState = new AnchorViewDefine.AnchorState();
        initExternalState();
    }

    public static void disableHeaderLiveData(boolean disable) {
        if (Boolean.TRUE.equals(AnchorConfig.disableHeaderLiveData.getValue()) == disable) {
            return;
        }
        AnchorConfig.disableHeaderLiveData.setValue(disable);
    }

    public static void disableHeaderVisitorCnt(boolean disable) {
        if (Boolean.TRUE.equals(AnchorConfig.disableHeaderVisitorCnt.getValue()) == disable) {
            return;
        }
        AnchorConfig.disableHeaderVisitorCnt.setValue(disable);
    }

    public static void disableFooterCoGuest(boolean disable) {
        if (Boolean.TRUE.equals(AnchorConfig.disableFooterCoGuest.getValue()) == disable) {
            return;
        }
        AnchorConfig.disableFooterCoGuest.setValue(disable);
    }

    public static void disableFooterCoHost(boolean disable) {
        if (Boolean.TRUE.equals(AnchorConfig.disableFooterCoHost.getValue()) == disable) {
            return;
        }
        AnchorConfig.disableFooterCoHost.setValue(disable);
    }

    public static void disableFooterBattle(boolean disable) {
        if (Boolean.TRUE.equals(AnchorConfig.disableFooterBattle.getValue()) == disable) {
            return;
        }
        AnchorConfig.disableFooterBattle.setValue(disable);
    }

    public static void disableFooterSoundEffect(boolean disable) {
        if (Boolean.TRUE.equals(AnchorConfig.disableFooterSoundEffect.getValue()) == disable) {
            return;
        }
        AnchorConfig.disableFooterSoundEffect.setValue(disable);
    }

    public void addObserver() {
        mLiveService.addRoomEngineObserver(mRoomEngineObserver);
        mLiveService.addLiveListManagerObserver(mLiveListManagerObserver);
        mLiveService.addFriendListener(mIMFriendshipListener);
    }

    public void removeObserver() {
        mLiveService.removeRoomEngineObserver(mRoomEngineObserver);
        mLiveService.removeLiveListManagerObserver(mLiveListManagerObserver);
        mLiveService.removeFriendListener(mIMFriendshipListener);
    }

    public void destroy() {
        removeObserver();
        mRoomManager.destroy();
        mCoGuestManager.destroy();
        mUserManager.destroy();
        mMediaManager.destroy();
        mCoHostManager.destroy();
        mBattleManager.destroy();
        mListenerManager.clearAnchorViewListeners();
    }

    public IAnchorAPI getLiveService() {
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

    public AnchorState getState() {
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

    public CoreState getCoreState() {
        return mCoreState;
    }

    public void setCoreState(CoreState coreState) {
        mCoreState = coreState;
        mRoomManager.setCoreState(coreState);
        mMediaManager.setCoreState(coreState);
        mUserManager.setCoreState(coreState);
        mCoGuestManager.setCoreState(coreState);
        mCoHostManager.setCoreState(coreState);
        mBattleManager.setCoreState(coreState);
    }

    public void setRoomId(String roomId) {
        getRoomState().roomId = roomId;
        LOGGER.info(hashCode() + " setRoomId:[mRoomId=" + roomId + ",mLiveService:" + mLiveService.hashCode()
                + ",mLiveObserver:" + mRoomEngineObserver.hashCode() + "]");
    }

    public void enablePipMode(boolean enable) {
        mMediaManager.enablePipMode(enable);
    }

    public AnchorViewDefine.AnchorState getExternalState() {
        return mExternalState;
    }

    public void setExternalState(int messageCount) {
        mExternalState.duration = System.currentTimeMillis() - getRoomState().liveInfo.createTime;
        mExternalState.messageCount = messageCount;
    }

    public void setLiveStatisticsData(TUILiveListManager.LiveStatisticsData data) {
        if (data == null) {
            return;
        }
        mExternalState.viewCount = data.totalViewers;
        mExternalState.giftSenderCount = data.totalUniqueGiftSenders;
        mExternalState.giftIncome = data.totalGiftCoins;
        mExternalState.likeCount = data.totalLikesReceived;
    }

    private void initExternalState() {
        mExternalState.duration = 0;
        mExternalState.viewCount = 0;
        mExternalState.messageCount = 0;
    }

    public void notifyPictureInPictureClick() {
        mListenerManager.notifyAnchorViewListener(AnchorViewDefine.AnchorViewListener::onClickFloatWindow);
    }

    public void notifyRoomExit() {
        mListenerManager.notifyAnchorViewListener(observer -> observer.onEndLiving(mExternalState));
    }

    public void addAnchorViewListener(AnchorViewDefine.AnchorViewListener listener) {
        mListenerManager.addAnchorViewListener(listener);
    }

    public void removeAnchorViewListener(AnchorViewDefine.AnchorViewListener listener) {
        mListenerManager.removeAnchorViewListener(listener);
    }

    public void setLiveStateListener(LiveStateListener listener) {
        mLiveStateListener = listener;
    }

    public void onRoomDismissed(String roomId) {
        if (mLiveStateListener != null) {
            mLiveStateListener.onRoomDismissed();
        }
    }

    public void onKickedOffLine(String message) {
        if (mLiveStateListener != null) {
            mLiveStateListener.onKickedOffLine(message);
        }
    }

    public void onKickedOutOfRoom(String roomId, TUIRoomDefine.KickedOutOfRoomReason reason, String message) {
        if (mLiveStateListener != null) {
            mLiveStateListener.onKickedOutOfRoom(roomId, reason, message);
        }
    }

    public interface LiveStateListener {
        void onRoomDismissed();

        void onKickedOffLine(String message);

        void onKickedOutOfRoom(String roomId, TUIRoomDefine.KickedOutOfRoomReason reason, String message);
    }
}
