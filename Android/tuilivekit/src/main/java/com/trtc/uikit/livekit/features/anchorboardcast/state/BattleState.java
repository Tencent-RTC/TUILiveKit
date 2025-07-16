package com.trtc.uikit.livekit.features.anchorboardcast.state;

import android.graphics.Rect;

import androidx.annotation.Nullable;
import androidx.lifecycle.MutableLiveData;

import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager;

import java.util.ArrayList;
import java.util.List;

public final class BattleState {

    public static final int BATTLE_REQUEST_TIMEOUT   = 10;
    public static final int BATTLE_DURATION          = 30;
    public static final int BATTLE_END_INFO_DURATION = 5;

    public final MutableLiveData<List<BattleUser>> mBattledUsers          = new MutableLiveData<>(new ArrayList<>());
    public final MutableLiveData<List<String>>     mSentBattleRequests    = new MutableLiveData<>(new ArrayList<>());
    public final MutableLiveData<BattleUser>       mReceivedBattleRequest = new MutableLiveData<>();
    public final MutableLiveData<Boolean>          mIsInWaiting           = new MutableLiveData<>(null);
    public final MutableLiveData<Boolean>          mIsBattleRunning       = new MutableLiveData<>(null);
    public final MutableLiveData<Boolean>          mIsOnDisplayResult     = new MutableLiveData<>(null);
    public final MutableLiveData<Integer>          mDurationCountDown     = new MutableLiveData<>(0);
    public final BattleConfig                      mBattleConfig          = new BattleConfig();
    public       String                            mBattleId              = "";
    public       boolean                           mIsShowingStartView    = false;

    public static class BattleUser {
        public String roomId;
        public String userId;
        public String userName;
        public String avatarUrl;
        public int    score;
        public int    ranking;
        public Rect   rect = new Rect();

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
