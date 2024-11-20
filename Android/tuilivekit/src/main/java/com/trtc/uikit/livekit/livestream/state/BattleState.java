package com.trtc.uikit.livekit.livestream.state;

import androidx.annotation.Nullable;

import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager;
import com.trtc.tuikit.common.livedata.LiveData;

import java.util.ArrayList;
import java.util.List;

public final class BattleState {

    public static final int BATTLE_REQUEST_TIMEOUT   = 10;
    public static final int BATTLE_DURATION          = 30;
    public static final int BATTLE_END_INFO_DURATION = 5;

    public final LiveData<List<BattleUser>> mBattledUsers          = new LiveData<>(new ArrayList<>());
    public final LiveData<List<BattleUser>> mSentBattleRequests    = new LiveData<>(new ArrayList<>());
    public final LiveData<BattleUser>       mReceivedBattleRequest = new LiveData<>();
    public final LiveData<Boolean>          mIsInWaiting           = new LiveData<>(null);
    public final LiveData<Boolean>          mIsBattleRunning       = new LiveData<>(null);
    public final LiveData<Boolean>          mIsOnDisplayResult     = new LiveData<>(null);
    public final LiveData<Integer>          mDurationCountDown     = new LiveData<>(0);
    public final BattleConfig               mBattleConfig          = new BattleConfig();
    public       String                     mBattleId              = "";
    public       boolean                    mNeedResponse          = true;
    public       boolean                    mIsShowingStartView    = false;

    public void reset() {
        mBattledUsers.get().clear();
        mSentBattleRequests.get().clear();
        mReceivedBattleRequest.set(null);
        mIsInWaiting.set(null);
        mIsBattleRunning.set(null);
        mIsOnDisplayResult.set(null);
        mDurationCountDown.set(0, false);
        mBattleConfig.reset();
        mBattleId = "";
        mIsShowingStartView = false;
    }

    public static class BattleUser {
        public String roomId;
        public String userId;
        public String userName;
        public String avatarUrl;
        public int    score;
        public int    ranking;

        public BattleUser() {
        }

        public BattleUser(TUILiveBattleManager.BattleUser battleUser) {
            roomId = battleUser.roomId;
            userId = battleUser.userId;
            userName = battleUser.userName;
            avatarUrl = battleUser.avatarUrl;
            score = battleUser.score;
        }

        public BattleUser(CoHostState.ConnectionUser user) {
            roomId = user.roomId;
            userId = user.userId;
            userName = user.userName;
            avatarUrl = user.avatarUrl;
        }

        @Override
        public boolean equals(@Nullable Object obj) {
            if (obj instanceof BattleUser) {
                return this.userId.equals(((BattleUser) obj).userId);
            }
            return false;
        }
    }

    public static class BattleConfig {
        public int     duration;
        public boolean needResponse;
        public String  extensionInfo;

        public BattleConfig() {
        }

        public void copy(TUILiveBattleManager.BattleConfig config) {
            duration = config.duration;
            needResponse = config.needResponse;
            extensionInfo = config.extensionInfo;
        }

        public void reset() {
            duration = 0;
            needResponse = true;
            extensionInfo = "";
        }
    }
}
