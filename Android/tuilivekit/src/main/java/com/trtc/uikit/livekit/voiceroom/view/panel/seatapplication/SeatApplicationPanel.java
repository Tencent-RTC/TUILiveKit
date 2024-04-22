package com.trtc.uikit.livekit.voiceroom.view.panel.seatapplication;

import android.content.Context;

import androidx.core.content.ContextCompat;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveController;

public class SeatApplicationPanel extends PopupDialog {

    public SeatApplicationPanel(Context context, LiveController liveController) {
        super(context);
        SeatApplicationView seatApplicationView = new SeatApplicationView(context, liveController);
        seatApplicationView.setBackground(ContextCompat.getDrawable(context, R.drawable.livekit_dialog_background));
        setView(seatApplicationView);
        seatApplicationView.setOnBackButtonClickListener(this::dismiss);
    }
}

