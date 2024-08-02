package com.trtc.uikit.livekit.common.uicomponent.beauty;


import android.content.Context;
import android.view.View;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.view.liveroom.view.anchor.component.common.BeautyListPanel;

public class BeautyViewFactory {

    public View getBeautyView(Context context, LiveController liveController) {
        View beautyView = liveController.getMediaController().getTEBeautyView(context);
        if (beautyView == null) {
            beautyView = new BeautyListPanel(context, liveController);
        } else {
            beautyView.setBackgroundResource(R.color.livekit_design_standard_g1);
        }
        return beautyView;
    }
}
