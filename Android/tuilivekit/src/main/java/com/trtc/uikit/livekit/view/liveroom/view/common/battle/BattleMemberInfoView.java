package com.trtc.uikit.livekit.view.liveroom.view.common.battle;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.state.operation.BattleState.BattleUser;

/**
 * mode: multi member Battle
 * for member
 */
@SuppressLint("ViewConstructor")
public class BattleMemberInfoView extends FrameLayout {

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

    private final ImageView mRankingView;
    private final TextView  mScoreView;
    private final TextView  mConnectionStatusView;

    public BattleMemberInfoView(@NonNull Context context) {
        this(context, null);
    }

    public BattleMemberInfoView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        inflate(context, R.layout.livekit_battle_member_info_view, this);
        mRankingView = findViewById(R.id.iv_ranking);
        mScoreView = findViewById(R.id.tv_score);
        mConnectionStatusView = findViewById(R.id.tv_connection_status);
        reset();
    }

    public void setData(BattleUser user) {
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

    public void reset() {
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
}
