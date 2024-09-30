package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming.battle;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.common.view.StandardDialog;
import com.trtc.uikit.livekit.manager.LiveController;

@SuppressLint("ViewConstructor")
public class AnchorEndBattlePanel extends BasicView {

    protected Context         mContext;
    private   OnClickListener mEndBattleListener;
    private   OnClickListener mCancelListener;

    public AnchorEndBattlePanel(@NonNull Context context, @NonNull LiveController liveController) {
        super(context, liveController);
        mContext = context;
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_end_battle_panel, this, true);
        initEndLiveButton();
        initCancelButton();
    }

    public void setEndBattleListener(OnClickListener listener) {
        mEndBattleListener = listener;
    }

    public void setCancelListener(OnClickListener listener) {
        mCancelListener = listener;
    }

    @Override
    protected void addObserver() {

    }

    @Override
    protected void removeObserver() {

    }


    private void initEndLiveButton() {
        TextView textEndLive = findViewById(R.id.tv_end_live);
        textEndLive.setText(getContext().getText(R.string.livekit_battle_end_pk));
        textEndLive.setOnClickListener(v -> {
            if (mEndBattleListener != null) {
                mEndBattleListener.onClick(v);
            }
            showEndBattleDialog();
        });
    }

    private void initCancelButton() {
        TextView textCancel = findViewById(R.id.tv_cancel);
        textCancel.setText(getContext().getText(R.string.livekit_cancel));
        textCancel.setOnClickListener(mCancelListener);
    }

    private void showEndBattleDialog() {
        StandardDialog dialog = new StandardDialog(getContext());
        dialog.setContent(getContext().getString(R.string.livekit_battle_end_pk_tips));
        dialog.setAvatar(null);
        dialog.setPositiveTextColor(mContext.getResources().getColor(R.color.livekit_not_standard_red));
        dialog.setNegativeText(getContext().getString(R.string.livekit_disconnect_cancel),
                negativeView -> {
                    dialog.dismiss();
                });
        dialog.setPositiveText(getContext().getString(R.string.livekit_battle_end_pk), positiveView -> {
            dialog.dismiss();
            mBattleController.exitBattle();
        });
        dialog.show();
    }
}
