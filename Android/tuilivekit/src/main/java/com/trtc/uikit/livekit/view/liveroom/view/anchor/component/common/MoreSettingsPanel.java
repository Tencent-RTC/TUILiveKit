package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.common;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;

@SuppressLint("ViewConstructor")
public class MoreSettingsPanel extends BasicView {

    private final PopupDialog.DialogActionListener mDismissListener;

    public MoreSettingsPanel(Context context, LiveController liveController,
                             PopupDialog.DialogActionListener listener) {
        super(context, liveController);
        mDismissListener = listener;
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_settings_more_panel, this, true);

        initBackView();
    }

    private void initBackView() {
        findViewById(R.id.iv_back).setOnClickListener((view) -> {
            if (mDismissListener != null) {
                mDismissListener.dismiss();
            }
        });
    }

    @Override
    protected void addObserver() {
    }

    @Override
    protected void removeObserver() {
    }
}
