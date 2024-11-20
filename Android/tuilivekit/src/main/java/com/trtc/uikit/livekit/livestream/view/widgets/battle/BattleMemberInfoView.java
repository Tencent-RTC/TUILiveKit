package com.trtc.uikit.livekit.livestream.view.widgets.battle;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.manager.api.LiveStreamLog;
import com.trtc.uikit.livekit.livestream.state.BattleState.BattleUser;
import com.trtc.uikit.livekit.livestream.state.CoHostState;
import com.trtc.uikit.livekit.livestream.state.CoHostState.ConnectionUser;
import com.trtc.uikit.livekit.livestream.view.BasicView;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * mode: multi member Battle
 * for member
 */
@SuppressLint("ViewConstructor")
public class BattleMemberInfoView extends BasicView {

    private static final String TAG = "BattleMemberInfoView";

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
    private final Observer<Boolean>              mFloatWindowModeObserver     = this::onFloatWindowModeObserver;

    private String mUserId;

    public BattleMemberInfoView(@NonNull Context context) {
        this(context, null);
    }

    public BattleMemberInfoView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public void init(LiveStreamManager liveStreamManager, String userId) {
        mUserId = userId;
        init(liveStreamManager);
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
    protected void refreshView() {
    }

    @Override
    protected void addObserver() {
        mCoHostState.connectedUsers.observe(mConnectionListObserver);
        mBattleState.mIsBattleRunning.observe(mBattleStartObserver);
        mBattleState.mBattledUsers.observe(mBattledListObserver);
        mBattleState.mIsOnDisplayResult.observe(mBattleResultDisplayObserver);
        FloatWindowManager.getInstance().getStore().isShowingFloatWindow.observe(mFloatWindowModeObserver);
    }

    @Override
    protected void removeObserver() {
        mCoHostState.connectedUsers.removeObserver(mConnectionListObserver);
        mBattleState.mIsBattleRunning.removeObserver(mBattleStartObserver);
        mBattleState.mBattledUsers.removeObserver(mBattledListObserver);
        mBattleState.mIsOnDisplayResult.removeObserver(mBattleResultDisplayObserver);
        FloatWindowManager.getInstance().getStore().isShowingFloatWindow.removeObserver(mFloatWindowModeObserver);
    }

    private void setData(BattleUser user) {
        setVisibility(VISIBLE);
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
        setVisibility(VISIBLE);
        mRankingView.setVisibility(show ? VISIBLE : GONE);
        mScoreView.setVisibility(show ? VISIBLE : GONE);
        mConnectionStatusView.setVisibility(show ? GONE : VISIBLE);
    }

    private void onConnectedListChange(List<CoHostState.ConnectionUser> connectionUsers) {
        onBattleScoreChanged(mBattleState.mBattledUsers.get());
    }

    private void onBattleScoreChanged(List<BattleUser> battleUsers) {
        if (FloatWindowManager.getInstance().isShowingFloatWindow()) {
            return;
        }
        if (battleUsers.isEmpty() || mCoHostState.connectedUsers.get().isEmpty()) {
            mBattleManager.resetOnDisplayResult();
            return;
        }
        final Map<String, BattleUser> battleUserMap = new HashMap<>();
        for (BattleUser user : mBattleState.mBattledUsers.get()) {
            battleUserMap.put(user.userId, user);
        }
        // single battle: only 2 users in connecting and battling (1v1 battle)
        final Map<String, BattleUser> singleBattleUserMap = new HashMap<>();
        if (mCoHostState.connectedUsers.get().size() == 2) {
            for (ConnectionUser connectionUser : mCoHostState.connectedUsers.get()) {
                BattleUser battleUser = battleUserMap.get(connectionUser.userId);
                if (battleUser != null) {
                    singleBattleUserMap.put(battleUser.userId, battleUser);
                }
            }
        }
        boolean isSingleBattle = singleBattleUserMap.size() == 2;
        LiveStreamLog.info(TAG + " onBattleChanged isSingleBattle: " + isSingleBattle);
        if (isSingleBattle) {
            reset();
        } else {
            setData(battleUserMap.get(mUserId));
        }
    }

    private void onBattleStartChange(Boolean start) {
        if (FloatWindowManager.getInstance().isShowingFloatWindow()) {
            return;
        }
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
        if (mCoHostState.connectedUsers.get().isEmpty()) {
            return;
        }
        final Map<String, BattleUser> battleUserMap = new HashMap<>();
        for (BattleUser user : mBattleState.mBattledUsers.get()) {
            battleUserMap.put(user.userId, user);
        }
        // single battle: only 2 users in connecting and battling (1v1 battle)
        final Map<String, BattleUser> singleBattleUserMap = new HashMap<>();
        if (mCoHostState.connectedUsers.get().size() == 2) {
            for (ConnectionUser connectionUser : mCoHostState.connectedUsers.get()) {
                BattleUser battleUser = battleUserMap.get(connectionUser.userId);
                if (battleUser != null) {
                    singleBattleUserMap.put(battleUser.userId, battleUser);
                }
            }
        }
        boolean isSingleBattle = singleBattleUserMap.size() == 2;
        LiveStreamLog.info(TAG + " onBattleEnd isSingleBattle: " + isSingleBattle);
        if (!isSingleBattle) {
            setData(battleUserMap.get(mUserId));
        }
    }

    private void onBattleResultDisplay(Boolean display) {
        if (Boolean.FALSE.equals(display)) {
            reset();
        }
    }

    private void onFloatWindowModeObserver(Boolean isFloating) {
        if (Boolean.TRUE.equals(isFloating)) {
            setVisibility(GONE);
        } else {
            setVisibility(VISIBLE);
        }
    }
}
