package com.trtc.uikit.livekit.livestream.view.anchor.pushing.battle;

import android.content.Context;
import android.os.Bundle;
import android.view.View;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.cohost.StandardDialog;

public final class AnchorEndBattleDialog extends PopupDialog {

    private final LiveStreamManager mLiveManager;

    public AnchorEndBattleDialog(@NonNull Context context, LiveStreamManager manager) {
        super(context, com.trtc.tuikit.common.R.style.TUICommonBottomDialogTheme);
        mLiveManager = manager;
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
        textEndLive.setText(getContext().getString(R.string.livekit_battle_end_pk));
        textEndLive.setOnClickListener(v -> {
            dismiss();
            showEndBattleDialog();
        });
    }

    private void initCancelButton() {
        TextView textCancel = findViewById(R.id.tv_cancel);
        textCancel.setText(getContext().getString(R.string.livekit_cancel));
        textCancel.setOnClickListener(v -> dismiss());
    }

    private void showEndBattleDialog() {
        Context context = getContext();
        StandardDialog dialog = new StandardDialog(getContext());
        dialog.setContent(getContext().getString(R.string.livekit_battle_end_pk_tips));
        dialog.setAvatar(null);
        dialog.setPositiveTextColor(context.getResources().getColor(R.color.livekit_not_standard_red));
        dialog.setNegativeText(getContext().getString(R.string.livekit_disconnect_cancel),
                negativeView -> dialog.dismiss());
        dialog.setPositiveText(getContext().getString(R.string.livekit_battle_end_pk), positiveView -> {
            dialog.dismiss();
            mLiveManager.getBattleManager().exitBattle();
        });
        dialog.show();
    }
}
