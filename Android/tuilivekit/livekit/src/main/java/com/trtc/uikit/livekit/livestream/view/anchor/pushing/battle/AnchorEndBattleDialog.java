package com.trtc.uikit.livekit.livestream.view.anchor.pushing.battle;

import android.content.Context;
import android.os.Bundle;
import android.view.View;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.manager.api.LiveStreamLog;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.cohost.StandardDialog;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

public final class AnchorEndBattleDialog extends PopupDialog {

    private final LiveStreamManager mLiveManager;
    private final LiveCoreView      mLiveCoreView;

    public AnchorEndBattleDialog(@NonNull Context context, LiveStreamManager manager, LiveCoreView liveCoreView) {
        super(context, com.trtc.tuikit.common.R.style.TUICommonBottomDialogTheme);
        mLiveManager = manager;
        mLiveCoreView = liveCoreView;
        View view = getLayoutInflater().inflate(R.layout.livekit_anchor_end_battle_panel, null);
        setView(view);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        initCancelButton();
        initEndLiveButton();
    }

    private void initEndLiveButton() {
        TextView textEndLive = findViewById(R.id.tv_end_live);
        textEndLive.setText(getContext().getString(R.string.live_battle_end_pk));
        textEndLive.setOnClickListener(v -> {
            dismiss();
            showEndBattleDialog();
        });
    }

    private void initCancelButton() {
        TextView textCancel = findViewById(R.id.tv_cancel);
        textCancel.setText(getContext().getString(R.string.live_cancel));
        textCancel.setOnClickListener(v -> dismiss());
    }

    private void showEndBattleDialog() {
        Context context = getContext();
        StandardDialog dialog = new StandardDialog(getContext());
        dialog.setContent(getContext().getString(R.string.live_battle_end_pk_tips));
        dialog.setAvatar(null);
        dialog.setPositiveTextColor(context.getResources().getColor(R.color.livekit_not_standard_red));
        dialog.setNegativeText(getContext().getString(R.string.live_disconnect_cancel),
                negativeView -> dialog.dismiss());
        dialog.setPositiveText(getContext().getString(R.string.live_battle_end_pk), positiveView -> {
            dialog.dismiss();
            String battleId = mLiveManager.getBattleState().mBattleId;
            mLiveCoreView.terminateBattle(battleId, new TUIRoomDefine.ActionCallback() {
                @Override
                public void onSuccess() {
                    mLiveManager.getBattleManager().onExitBattle();
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    LiveStreamLog.error("AnchorEndBattleDialog" + " terminateBattle failed:error:" + error + "," +
                            "errorCode:" + error.getValue() + "message:" + message);
                    ErrorLocalized.onError(error);
                }
            });
        });
        dialog.show();
    }
}
