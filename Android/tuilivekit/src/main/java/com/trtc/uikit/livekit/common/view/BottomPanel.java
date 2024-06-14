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

    private void setTransparentBackground(BottomPanelView bottomPanel) {
        bottomPanel.setBackgroundResource(com.trtc.tuikit.common.R.color.common_design_bottom_sheet_color);
        setView(bottomPanel);
        bottomPanel.setOnBackButtonClickListener(this::dismiss);
    }

    public static BottomPanel create(BottomPanelView bottomPanel) {
        BottomPanel panel = new BottomPanel(bottomPanel.getContext());
        panel.initView(bottomPanel);
        return panel;
    }

    public static BottomPanel createTransparent(BottomPanelView bottomPanel) {
        BottomPanel panel = new BottomPanel(bottomPanel.getContext());
        panel.setTransparentBackground(bottomPanel);
        return panel;
    }
}
