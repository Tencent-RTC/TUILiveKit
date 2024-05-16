package com.trtc.uikit.livekit.common.view;

import android.content.Context;

import androidx.core.content.ContextCompat;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;

public class BottomPanel extends PopupDialog {
    private BottomPanel(Context context) {
        super(context);
    }

    private void initView(BottomPanelView bottomPanel) {
        bottomPanel.setBackground(ContextCompat.getDrawable(bottomPanel.getContext(),
                R.drawable.livekit_dialog_background));
        setView(bottomPanel);
        bottomPanel.setOnBackButtonClickListener(this::dismiss);
    }

    public static BottomPanel create(BottomPanelView bottomPanel) {
        BottomPanel panel = new BottomPanel(bottomPanel.getContext());
        panel.initView(bottomPanel);
        return panel;
    }
}
