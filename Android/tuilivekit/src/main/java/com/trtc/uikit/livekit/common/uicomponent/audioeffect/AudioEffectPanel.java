package com.trtc.uikit.livekit.common.uicomponent.audioeffect;

import android.content.Context;

import androidx.core.content.ContextCompat;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveController;

public class AudioEffectPanel extends PopupDialog {

    public AudioEffectPanel(Context context, LiveController liveController) {
        super(context);
        AudioEffectView audioEffectPanelView = new AudioEffectView(context, liveController);
        audioEffectPanelView.setBackground(ContextCompat.getDrawable(context, R.drawable.livekit_dialog_background));
        setView(audioEffectPanelView);
        audioEffectPanelView.setOnBackButtonClickListener(this::dismiss);
    }
}

