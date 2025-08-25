package com.trtc.uikit.livekit.features.audiencecontainer.view.battle.widgets;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.features.audiencecontainer.state.BattleState.BattleUser;
import com.trtc.uikit.livekit.features.audiencecontainer.view.BasicView;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@SuppressLint("ViewConstructor")
public class BattleInfoView extends BasicView {
    private enum BattleResultType {
        DRAW, VICTORY, DEFEAT
    }

    private SingleBattleScoreView mSingleBattleScoreView;
    private TextView              mBattleTimeView;
    private ImageView             mBattleStartView;
    private ImageView             mBattleResultView;

    private final Observer<List<ConnectionUser>> mConnectionListObserver      = this::onConnectedListChange;
    private final Observer<List<BattleUser>>     mBattledListObserver         = this::onBattleScoreChanged;
    private final Observer<Boolean>              mBattleStartObserver         = this::onBattleStartChange;
    private final Observer<Integer>              mDurationCountDownObserver   = this::onDurationCountDown;
    private final Observer<Boolean>              mBattleResultDisplayObserver = this::onResultDisplay;
    private final Observer<Boolean>              mPictureInPictureObserver    = this::onPictureInPictureObserver;


    public BattleInfoView(@NonNull Context context) {
        this(context, null);
    }

    public BattleInfoView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    @Override
    protected void initView() {
        inflate(getContext(), R.layout.livekit_audience_battle_info_view, this);
        mBattleTimeView = findViewById(R.id.tv_battle_time);
        mBattleStartView = findViewById(R.id.iv_battle_start);
        mBattleResultView = findViewById(R.id.iv_battle_result);
        mSingleBattleScoreView = findViewById(R.id.single_battle_score_view);
        setVisibility(GONE);
    }

    @Override
    protected void addObserver() {
        mCoreState.coHostState.connectedUserList.observeForever(mConnectionListObserver);
        mBattleState.mIsBattleRunning.observeForever(mBattleStartObserver);
        mBattleState.mBattledUsers.observeForever(mBattledListObserver);
        mBattleState.mDurationCountDown.observeForever(mDurationCountDownObserver);
        mBattleState.mIsOnDisplayResult.observeForever(mBattleResultDisplayObserver);
        mMediaState.isPictureInPictureMode.observeForever(mPictureInPictureObserver);
    }

    @Override
    protected void removeObserver() {
        mCoreState.coHostState.connectedUserList.removeObserver(mConnectionListObserver);
        mBattleState.mIsBattleRunning.removeObserver(mBattleStartObserver);
        mBattleState.mBattledUsers.removeObserver(mBattledListObserver);
        mBattleState.mDurationCountDown.removeObserver(mDurationCountDownObserver);
        mBattleState.mIsOnDisplayResult.removeObserver(mBattleResultDisplayObserver);
        mMediaState.isPictureInPictureMode.removeObserver(mPictureInPictureObserver);
    }

    public void updateView(List<LiveCoreViewDefine.BattleUserViewModel> userInfos) {
        List<BattleUser> battledUsers = mBattleState.mBattledUsers.getValue();
        for (LiveCoreViewDefine.BattleUserViewModel model : userInfos) {
            for (BattleUser battleUser : battledUsers) {
                if (TextUtils.equals(battleUser.userId, model.battleUser.userId)) {
                    battleUser.rect.set(model.rect);
                    break;
                }
            }
        }
        onBattleScoreChanged();
    }

    private void onBattleStart() {
        mSingleBattleScoreView.setVisibility(GONE);
        setVisibility(Boolean.TRUE.equals(mMediaState.isPictureInPictureMode.getValue()) ? GONE : VISIBLE);
        if (mCoreState.userState.selfInfo.getValue().userRole == TUIRoomDefine.Role.ROOM_OWNER && !mBattleState.mIsShowingStartView) {
            mBattleStartView.setVisibility(VISIBLE);
            postDelayed(() -> mBattleStartView.setVisibility(GONE), 1000);
        }
    }

    private void onBattleEnd() {
        setVisibility(Boolean.TRUE.equals(mMediaState.isPictureInPictureMode.getValue()) ? GONE : VISIBLE);
        mBattleTimeView.setText(mContext.getString(R.string.common_battle_pk_end));
    }

    private void onBattleScoreChanged() {
        List<BattleUser> users = mBattleState.mBattledUsers.getValue();
        if (users.isEmpty()) {
            return;
        }
        final Map<String, BattleUser> battleUserMap = new HashMap<>();
        for (BattleUser user : users) {
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
        if (isSingleBattle) {
            List<BattleUser> userList = new ArrayList<>(singleBattleUserMap.values());
            if (userList.get(0).rect.left < userList.get(1).rect.left) {
                updateData(userList.get(0), userList.get(1));
            } else {
                updateData(userList.get(1), userList.get(0));
            }
        }
    }

    private void updateData(BattleUser inviter, BattleUser invitee) {
        mSingleBattleScoreView.setVisibility(VISIBLE);
        mSingleBattleScoreView.updateScores(inviter.score, invitee.score);
    }

    private void showBattleResult(BattleResultType type) {
        mBattleResultView.setVisibility(VISIBLE);
        int resId = R.drawable.livekit_battle_result_draw;
        if (type == BattleResultType.VICTORY) {
            resId = R.drawable.livekit_battle_result_victory;
        } else if (type == BattleResultType.DEFEAT) {
            resId = R.drawable.livekit_battle_result_defeat;
        }
        mBattleResultView.setImageResource(resId);
    }

    private void onDurationCountDown(int duration) {
        post(() -> updateTime(duration));
    }

    @SuppressLint("DefaultLocale")
    private void updateTime(long time) {
        mBattleTimeView.setText(String.format("%d:%02d", time / 60, time % 60));
    }

    private void onResultDisplay(Boolean display) {
        if (Boolean.TRUE.equals(display)) {
            for (BattleUser battleUser : mBattleState.mBattledUsers.getValue()) {
                if (battleUser.userId.equals(mRoomState.liveInfo.ownerId)) {
                    BattleResultType type = mBattleManager.isBattleDraw()
                            ? BattleResultType.DRAW
                            : (battleUser.ranking == 1 ? BattleResultType.VICTORY : BattleResultType.DEFEAT);
                    showBattleResult(type);
                    break;
                }
            }
        } else if (Boolean.FALSE.equals(display)) {
            stopDisplayBattleResult();
        }
    }

    private void stopDisplayBattleResult() {
        updateTime(0);
        setVisibility(GONE);
        mBattleStartView.setVisibility(GONE);
        mBattleResultView.setVisibility(GONE);
    }

    private void onConnectedListChange(List<ConnectionUser> connectionUsers) {
        onBattleScoreChanged(mBattleState.mBattledUsers.getValue());
    }

    private void onBattleScoreChanged(List<BattleUser> battleUsers) {
        if (battleUsers.isEmpty() || mCoreState.coHostState.connectedUserList.getValue().isEmpty()) {
            mBattleManager.resetOnDisplayResult();
            return;
        }
        onBattleScoreChanged();
    }

    private void onBattleStartChange(Boolean start) {
        if (Boolean.TRUE.equals(start)) {
            onBattleStart();
        } else if (Boolean.FALSE.equals(start)) {
            onBattleEnd();
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

