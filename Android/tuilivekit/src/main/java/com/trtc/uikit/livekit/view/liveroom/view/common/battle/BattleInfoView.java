package com.trtc.uikit.livekit.view.liveroom.view.common.battle;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.gridlayout.widget.GridLayout;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.BattleState;
import com.trtc.uikit.livekit.state.operation.BattleState.BattleUser;
import com.trtc.uikit.livekit.state.operation.ConnectionState;
import com.trtc.uikit.livekit.view.liveroom.view.common.video.VideoView;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

@SuppressLint("ViewConstructor")
public class BattleInfoView extends BasicView {

    private static final String TAG = "BattleInfoView";

    private enum BattleResultType {
        DRAW, VICTORY, DEFEAT
    }

    private SingleBattleScoreView mSingleBattleScoreView;
    private TextView              mBattleTimeView;
    private ImageView             mBattleStartView;
    private ImageView             mBattleResultView;

    private final AtomicBoolean mNeedStopCountDown       = new AtomicBoolean(false);
    private final Runnable      mBattleEndInfoRemoveTask = this::stopDisplayBattleResult;

    public BattleInfoView(@NonNull Context context, @NonNull LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        inflate(getContext(), R.layout.livekit_battle_info_view, this);
        mBattleTimeView = findViewById(R.id.tv_battle_time);
        mBattleStartView = findViewById(R.id.iv_battle_start);
        mBattleResultView = findViewById(R.id.iv_battle_result);
        mSingleBattleScoreView = new SingleBattleScoreView(getContext());
        FrameLayout flBattleInfo = findViewById(R.id.fl_top);
        flBattleInfo.addView(mSingleBattleScoreView);
        mSingleBattleScoreView.setVisibility(GONE);
    }

    @Override
    protected void addObserver() {
    }

    @Override
    protected void removeObserver() {
        removeCallbacks(mBattleEndInfoRemoveTask);
    }

    public void onBattleStart() {
        mNeedStopCountDown.set(false);
        startCountdown();
    }

    public void onBattleEnd() {
        mNeedStopCountDown.set(true);
        for (BattleUser battleUser : mBattleState.mBattledUsers.get()) {
            if (battleUser.userId.equals(mRoomState.ownerInfo.userId)) {
                BattleResultType type = mBattleController.isBattleDraw()
                        ? BattleResultType.DRAW
                        : (battleUser.ranking == 1 ? BattleResultType.VICTORY : BattleResultType.DEFEAT);
                showBattleResult(type);
            }
        }
        postDelayed(mBattleEndInfoRemoveTask, BattleState.BATTLE_END_INFO_DURATION * 1000);
    }

    public void onBattleScoreChanged() {
        List<BattleUser> users = mBattleState.mBattledUsers.get();
        if (users.isEmpty()) {
            return;
        }
        final Map<String, BattleUser> battleUserMap = new HashMap<>();
        for (BattleUser user : users) {
            battleUserMap.put(user.userId, user);
        }
        // single battle: only 2 users in connecting and battling (1v1 battle)
        final Map<String, BattleUser> singleBattleUserMap = new HashMap<>();
        if (mConnectionState.connectedUsers.get().size() == 2) {
            for (ConnectionState.ConnectionUser connectionUser : mConnectionState.connectedUsers.get()) {
                BattleState.BattleUser battleUser = battleUserMap.get(connectionUser.userId);
                if (battleUser != null) {
                    singleBattleUserMap.put(battleUser.userId, battleUser);
                }
            }
        }
        boolean isSingleBattle = singleBattleUserMap.size() == 2;
        LiveKitLog.info(TAG + " onBattleScoreChanged isSingleBattle: " + isSingleBattle);
        if (isSingleBattle) {
            List<BattleUser> userList = new ArrayList<>(singleBattleUserMap.values());
            String ownerId = mRoomState.ownerInfo.userId;
            VideoView ownerVideoView = mLiveController.getVideoViewFactory().findVideoView(ownerId);
            GridLayout.LayoutParams layoutParams = (GridLayout.LayoutParams) ownerVideoView.getLayoutParams();
            boolean ownerOnLeft = layoutParams.leftMargin < layoutParams.width;
            if ((TextUtils.equals(userList.get(0).userId, ownerId) && ownerOnLeft)
                    || (TextUtils.equals(userList.get(1).userId, ownerId) && !ownerOnLeft)) {
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

    private void startCountdown() {
        mSingleBattleScoreView.setVisibility(GONE);
        setVisibility(VISIBLE);
        long[] duration = {mBattleState.mBattleConfig.duration};
        post(new Runnable() {
            @Override
            public void run() {
                if (duration[0] < 0 || Boolean.TRUE.equals(mNeedStopCountDown.get())) {
                    mBattleTimeView.setText(mContext.getString(R.string.livekit_battle_pk_end));
                    return;
                }
                updateTime(duration[0]);
                duration[0]--;
                postDelayed(this, 1000);
            }
        });
        if (mUserState.selfInfo.role.get() == TUIRoomDefine.Role.ROOM_OWNER) {
            mBattleStartView.setVisibility(VISIBLE);
            postDelayed(() -> mBattleStartView.setVisibility(GONE), 1000);
        }
    }

    @SuppressLint("DefaultLocale")
    private void updateTime(long time) {
        mBattleTimeView.setText(String.format("%d:%02d", time / 60, time % 60));
    }

    public void stopDisplayBattleResult() {
        if (mBattleStartView == null) {
            return;
        }
        removeCallbacks(mBattleEndInfoRemoveTask);
        mBattleController.resetOnDisplayResult();
        updateTime(0);
        setVisibility(GONE);
        mBattleStartView.setVisibility(GONE);
        mBattleResultView.setVisibility(GONE);
    }
}

