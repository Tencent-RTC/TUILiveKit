package com.trtc.uikit.livekit.livestream.view.anchor.pushing.battle;

import android.app.Dialog;
import android.content.Context;
import android.os.Bundle;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.state.BattleState;

public final class BattleCountdownDialog extends Dialog {

    private final LiveStreamManager mLiveStreamManager;
    private       TextView          mCountdownView;
    private       int               mCountdownValue = BattleState.BATTLE_REQUEST_TIMEOUT;

    public BattleCountdownDialog(@NonNull Context context, LiveStreamManager liveStreamManager) {
        super(context, android.R.style.Theme_Translucent_NoTitleBar);
        mLiveStreamManager = liveStreamManager;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.livekit_battle_count_down_view);
        setCancelable(false);
        setCanceledOnTouchOutside(false);
        mCountdownView = findViewById(R.id.tv_countdown);
        findViewById(R.id.tv_cancel).setOnClickListener(view -> cancelBattle());
        startCountdown();
    }

    private void startCountdown() {
        mCountdownView.post(new Runnable() {
            @Override
            public void run() {
                mCountdownView.setText(String.valueOf(mCountdownValue));
                mCountdownValue--;
                if (mCountdownValue < 0) {
                    cancelBattle();
                    return;
                }
                mCountdownView.postDelayed(this, 1000);
            }
        });
    }

    private void cancelBattle() {
        mLiveStreamManager.getBattleManager().cancelBattleRequest();
    }
}
