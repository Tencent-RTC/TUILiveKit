package com.trtc.uikit.livekit.voiceroom.view.panel.audiencelist;

import android.content.Context;

import androidx.core.content.ContextCompat;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveController;

public class AudienceListPanel extends PopupDialog {

    public AudienceListPanel(Context context, LiveController liveController) {
        super(context);
        AudienceListPanelView mAudienceListPanelView = new AudienceListPanelView(context, liveController);
        mAudienceListPanelView.setBackground(ContextCompat.getDrawable(context, R.drawable.livekit_dialog_background));
        setView(mAudienceListPanelView);
        mAudienceListPanelView.setOnBackButtonClickListener(this::dismiss);
    }
}
