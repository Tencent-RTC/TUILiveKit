package com.trtc.uikit.livekit.features.audiencecontainer.view.battle.widgets;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.AudienceManager;
import com.trtc.uikit.livekit.features.audiencecontainer.state.BattleState.BattleUser;
import com.trtc.uikit.livekit.features.audiencecontainer.view.BasicView;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * mode: multi member Battle
 * for member
 */
@SuppressLint("ViewConstructor")
public class BattleMemberInfoView extends BasicView {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("BattleMemberInfoView");

    private static final int[] RANKING_IMAGE = {
            R.drawable.livekit_battle_ranking_1,
            R.drawable.livekit_battle_ranking_2,
            R.drawable.livekit_battle_ranking_3,
            R.drawable.livekit_battle_ranking_4,
            R.drawable.livekit_battle_ranking_5,
            R.drawable.livekit_battle_ranking_6,
            R.drawable.livekit_battle_ranking_7,
            R.drawable.livekit_battle_ranking_8,
            R.drawable.livekit_battle_ranking_9,
    };

    private ImageView mRankingView;
    private TextView  mScoreView;
    private TextView  mConnectionStatusView;

    private final Observer<List<ConnectionUser>> mConnectionListObserver      = this::onConnectedListChange;
    private final Observer<List<BattleUser>>     mBattledListObserver         = this::onBattleScoreChanged;
    private final Observer<Boolean>              mBattleStartObserver         = this::onBattleStartChange;
    private final Observer<Boolean>              mBattleResultDisplayObserver = this::onBattleResultDisplay;
    private final Observer<Boolean>              mPictureInPictureObserver    = this::onPictureInPictureObserver;

    private String mUserId;

    public BattleMemberInfoView(@NonNull Context context) {
        this(context, null);
    }

    public BattleMemberInfoView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public void init(AudienceManager audienceManager, String userId) {
        mUserId = userId;
        init(audienceManager);
    }

    @Override
    protected void initView() {
        inflate(getContext(), R.layout.livekit_battle_member_info_view, this);
        mRankingView = findViewById(R.id.iv_ranking);
        mScoreView = findViewById(R.id.tv_score);
        mConnectionStatusView = findViewById(R.id.tv_connection_status);
        reset();
    }

    @Override
    protected void addObserver() {
        mCoreState.coHostState.connectedUserList.observeForever(mConnectionListObserver);
        mBattleState.mIsBattleRunning.observeForever(mBattleStartObserver);
        mBattleState.mBattledUsers.observeForever(mBattledListObserver);
        mBattleState.mIsOnDisplayResult.observeForever(mBattleResultDisplayObserver);
        mMediaState.isPictureInPictureMode.observeForever(mPictureInPictureObserver);
    }

    @Override
    protected void removeObserver() {
        mCoreState.coHostState.connectedUserList.removeObserver(mConnectionListObserver);
        mBattleState.mIsBattleRunning.removeObserver(mBattleStartObserver);
        mBattleState.mBattledUsers.removeObserver(mBattledListObserver);
        mBattleState.mIsOnDisplayResult.removeObserver(mBattleResultDisplayObserver);
        mMediaState.isPictureInPictureMode.removeObserver(mPictureInPictureObserver);
    }

    private void setData(BattleUser user) {
        setVisibility(Boolean.TRUE.equals(mMediaState.isPictureInPictureMode.getValue()) ? GONE : VISIBLE);
        if (user == null) {
            showBattleView(false);
        } else {
            showBattleView(true);
            mScoreView.setText(String.valueOf(user.score));
            if (user.ranking > 0 && user.ranking <= RANKING_IMAGE.length) {
                mRankingView.setImageResource(RANKING_IMAGE[user.ranking - 1]);
            }
        }
    }

    private void reset() {
        setVisibility(GONE);
        mRankingView.setVisibility(GONE);
        mScoreView.setVisibility(GONE);
        mConnectionStatusView.setVisibility(GONE);
    }

    private void showBattleView(boolean show) {
        setVisibility(Boolean.TRUE.equals(mMediaState.isPictureInPictureMode.getValue()) ? GONE : VISIBLE);
        mRankingView.setVisibility(show ? VISIBLE : GONE);
        mScoreView.setVisibility(show ? VISIBLE : GONE);
        mConnectionStatusView.setVisibility(show ? GONE : VISIBLE);
    }

    private void onConnectedListChange(List<ConnectionUser> connectionUsers) {
        onBattleScoreChanged(mBattleState.mBattledUsers.getValue());
    }

    private void onBattleScoreChanged(List<BattleUser> battleUsers) {
        if (battleUsers.isEmpty() || mCoreState.coHostState.connectedUserList.getValue().isEmpty()) {
            mBattleManager.resetOnDisplayResult();
            return;
        }
        final Map<String, BattleUser> battleUserMap = new HashMap<>();
        for (BattleUser user : mBattleState.mBattledUsers.getValue()) {
            battleUserMap.put(user.userId, user);
        }
        // single battle: only 2 users in connecting and battling (1v1 battle)
        final Map<String, BattleUser> singleBattleUserMap = new HashMap<>();
        if (mCoreState.coHostState.connectedUserList.getValue().size() == 2) {
            for (ConnectionUser connectionUser : mCoreState.coHostState.connectedUserList.getValue()) {
                BattleUser battleUser = battleUserMap.get(connectionUser.userId);
                if (battleUser != null) {
                    singleBattleUserMap.put(battleUser.userId, battleUser);
                }
            }
        }
        boolean isSingleBattle = singleBattleUserMap.size() == 2;
        LOGGER.info("onBattleChanged isSingleBattle: " + isSingleBattle);
        if (isSingleBattle) {
            reset();
        } else {
            setData(battleUserMap.get(mUserId));
        }
    }

    private void onBattleStartChange(Boolean start) {
        if (Boolean.TRUE.equals(start)) {
            onBattleStart();
        } else if (Boolean.FALSE.equals(start)) {
            onBattleEnd();
        }
    }

    private void onBattleStart() {
        reset();
    }

    private void onBattleEnd() {
        if (mCoreState.coHostState.connectedUserList.getValue().isEmpty()) {
            return;
        }
        final Map<String, BattleUser> battleUserMap = new HashMap<>();
        for (BattleUser user : mBattleState.mBattledUsers.getValue()) {
            battleUserMap.put(user.userId, user);
        }
        // single battle: only 2 users in connecting and battling (1v1 battle)
        final Map<String, BattleUser> singleBattleUserMap = new HashMap<>();
        if (mCoreState.coHostState.connectedUserList.getValue().size() == 2) {
            for (ConnectionUser connectionUser : mCoreState.coHostState.connectedUserList.getValue()) {
                BattleUser battleUser = battleUserMap.get(connectionUser.userId);
                if (battleUser != null) {
                    singleBattleUserMap.put(battleUser.userId, battleUser);
                }
            }
        }
        boolean isSingleBattle = singleBattleUserMap.size() == 2;
        LOGGER.info("onBattleEnd isSingleBattle: " + isSingleBattle);
        if (!isSingleBattle) {
            setData(battleUserMap.get(mUserId));
        }
    }

    private void onBattleResultDisplay(Boolean display) {
        if (Boolean.FALSE.equals(display)) {
            reset();
        }
    }

    private void onPictureInPictureObserver(Boolean isPipMode) {
        if (Boolean.TRUE.equals(isPipMode)) {
            setVisibility(GONE);
        } else {
            if (Boolean.TRUE.equals(mBattleState.mIsBattleRunning.getValue())) {
                setVisibility(VISIBLE);
            }
        }
    }
}
