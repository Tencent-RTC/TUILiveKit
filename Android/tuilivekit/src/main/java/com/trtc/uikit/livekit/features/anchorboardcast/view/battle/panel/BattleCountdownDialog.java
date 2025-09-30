package com.trtc.uikit.livekit.features.anchorboardcast.view.battle.panel;

import android.app.Dialog;
import android.content.Context;
import android.os.Bundle;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;
import com.trtc.uikit.livekit.features.anchorboardcast.state.BattleState;
import io.trtc.tuikit.atomicxcore.api.LiveCoreView;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

public final class BattleCountdownDialog extends Dialog {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("BattleCountdownDialog");

    private final AnchorManager mAnchorManager;
    private final LiveCoreView  mLiveCoreView;
    private       TextView      mCountdownView;
    private       TextView      mTipView;
    private       int           mCountdownValue = BattleState.BATTLE_REQUEST_TIMEOUT;

    public BattleCountdownDialog(@NonNull Context context,
                                 AnchorManager liveStreamManager, LiveCoreView liveCoreView) {
        super(context, android.R.style.Theme_Translucent_NoTitleBar);
        mAnchorManager = liveStreamManager;
        mLiveCoreView = liveCoreView;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.livekit_battle_count_down_view);
        setCancelable(false);
        setCanceledOnTouchOutside(false);
        mCountdownView = findViewById(R.id.tv_countdown);
        mTipView = findViewById(R.id.tv_tip);
        findViewById(R.id.tv_cancel).setOnClickListener(view -> cancelBattle());
        startCountdown();
    }

    private void startCountdown() {
        mCountdownView.post(new Runnable() {
            @Override
            public void run() {
                mTipView.setText(formatTip());
                mCountdownView.setText(formatTime(mCountdownValue));
                mCountdownValue--;
                if (mCountdownValue < 0) {
                    dismiss();
                    return;
                }
                mCountdownView.postDelayed(this, 1000);
            }
        });
    }

    private String formatTip() {
        String tip = getContext().getString(R.string.common_battle_wait_start);
        StringBuilder tipBuilder = new StringBuilder(tip);
        for (int i = 0; i <= 2 - mCountdownValue % 3; i++) {
            tipBuilder.append(".");
        }
        return tipBuilder.toString();
    }

    private String formatTime(int second) {
        if (second <= 0) {
            return "00:00";
        } else if (second < 60) {
            return String.format(Locale.getDefault(), "00:%02d", second % 60);
        } else {
            return String.format(Locale.getDefault(), "%02d:%02d", second / 60, second % 60);
        }
    }

    private void cancelBattle() {
        List<String> list = new ArrayList<>();
        String selfId = mAnchorManager.getCoreState().userState.selfInfo.getValue().userId;
        for (ConnectionUser user : mAnchorManager.getCoreState().coHostState.connectedUserList.getValue()) {
            if (!user.userId.equals(selfId)) {
                list.add(user.userId);
            }
        }
        String battleId = mAnchorManager.getBattleState().mBattleId;
        mAnchorManager.getBattleManager().onCanceledBattle();
        mLiveCoreView.cancelBattle(battleId, list, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {

            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("BattleCountdownDialog" + " cancelBattle failed:error:" + error + "," +
                        "errorCode:" + error.getValue() + "message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }
}
