package com.trtc.uikit.livekit.livestream.view.anchor.pushing.battle;

import android.app.Dialog;
import android.content.Context;
import android.os.Bundle;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.state.BattleState;
import com.trtc.uikit.livekit.livestream.state.CoHostState;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

import java.util.ArrayList;
import java.util.List;

public final class BattleCountdownDialog extends Dialog {

    private final LiveStreamManager mLiveStreamManager;
    private final LiveCoreView      mLiveCoreView;
    private       TextView          mCountdownView;
    private       int               mCountdownValue = BattleState.BATTLE_REQUEST_TIMEOUT;

    public BattleCountdownDialog(@NonNull Context context,
                                 LiveStreamManager liveStreamManager, LiveCoreView liveCoreView) {
        super(context, android.R.style.Theme_Translucent_NoTitleBar);
        mLiveStreamManager = liveStreamManager;
        mLiveCoreView = liveCoreView;
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
        List<String> list = new ArrayList<>();
        String selfId = mLiveStreamManager.getUserState().selfInfo.userId;
        for (CoHostState.ConnectionUser user : mLiveStreamManager.getCoHostState().connectedUsers.get()) {
            if (!user.userId.equals(selfId)) {
                list.add(user.userId);
            }
        }
        String battleId = mLiveStreamManager.getBattleState().mBattleId;
        mLiveCoreView.cancelBattle(battleId, list, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mLiveStreamManager.getBattleManager().onCanceledBattle();
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {

            }
        });
    }
}
