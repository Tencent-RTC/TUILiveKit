package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;

@SuppressLint("ViewConstructor")
public class AnchorEndLivePanel extends BasicView {

    protected Context              mContext;
    private   boolean              mDisplay;
    private   String               mTips;
    private   String               mDisconnectText;
    private   View.OnClickListener mEndLiveListener;
    private   View.OnClickListener mCancelListener;
    private   View.OnClickListener mDisconnectListener;

    public AnchorEndLivePanel(@NonNull Context context, @NonNull LiveController liveController) {
        super(context, liveController);
        mContext = context;
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_end_live_panel, this, true);

        initTips();
        initExpandedPanel();
        initDisconnectButton();
        initEndLiveButton();
        initCancelButton();
    }

    public void configEndLivePanel(PopupDialog popupDialog) {
        boolean isConnected = !mConnectionState.connectedUsers.get().isEmpty();
        boolean isBattled = !mBattleState.mBattledUsers.get().isEmpty();
        setDisplayExpandedOption(isConnected);

        if (isBattled) {
            String tips = getContext().getString(R.string.livekit_end_pk_tips);
            String disconnectText = getContext().getString(R.string.livekit_end_pk);
            View.OnClickListener endExpandedOptionListener = v -> {
                mBattleController.exitBattle();
                popupDialog.dismiss();
            };
            setTips(tips);
            setDisconnectText(disconnectText, endExpandedOptionListener);
        } else if (isConnected) {
            String tips = getContext().getString(R.string.livekit_end_connection_tips);
            String disconnectText = getContext().getString(R.string.livekit_end_connection);
            View.OnClickListener endExpandedOptionListener = v -> {
                mConnectionController.disconnect();
                popupDialog.dismiss();
            };
            setTips(tips);
            setDisconnectText(disconnectText, endExpandedOptionListener);
        }
    }

    public void setEndLiveListener(View.OnClickListener listener) {
        mEndLiveListener = listener;
    }

    public void setCancelListener(View.OnClickListener listener) {
        mCancelListener = listener;
    }

    @Override
    protected void addObserver() {

    }

    @Override
    protected void removeObserver() {

    }

    private void setDisplayExpandedOption(boolean isDisplay) {
        mDisplay = isDisplay;
    }

    private void setTips(String message) {
        mTips = message;
    }

    private void setDisconnectText(String message, View.OnClickListener listener) {
        mDisconnectText = message;
        mDisconnectListener = listener;
    }

    private void initTips() {
        TextView textTips = findViewById(R.id.tv_tips);
        textTips.setText(mTips);
    }

    private void initExpandedPanel() {
        LinearLayout layoutPanel = findViewById(R.id.ll_options_panel);
        if (mDisplay) {
            layoutPanel.setVisibility(VISIBLE);
        } else {
            layoutPanel.setVisibility(GONE);
        }
    }

    private void initDisconnectButton() {
        TextView textDisconnect = findViewById(R.id.tv_disconnect);
        if (!TextUtils.isEmpty(mDisconnectText)) {
            textDisconnect.setText(mDisconnectText);
        }
        textDisconnect.setOnClickListener(mDisconnectListener);
    }

    private void initEndLiveButton() {
        TextView textEndLive = findViewById(R.id.tv_end_live);
        textEndLive.setText(getContext().getText(R.string.livekit_end_live));
        textEndLive.setOnClickListener(mEndLiveListener);
    }

    private void initCancelButton() {
        TextView textCancel = findViewById(R.id.tv_cancel);
        textCancel.setText(getContext().getText(R.string.livekit_cancel));
        textCancel.setOnClickListener(mCancelListener);
    }
}
