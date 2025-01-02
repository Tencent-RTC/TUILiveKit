package com.trtc.uikit.livekit.component.beauty;


import android.content.Context;
import android.view.View;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.view.widgets.beauty.BeautyListPanel;

public class BeautyViewFactory {

    public View getBeautyView(Context context, LiveStreamManager liveStreamManager) {
        View beautyView = liveStreamManager.getMediaManager().getTEBeautyView(context);
        if (beautyView == null) {
            beautyView = new BeautyListPanel(context);
            ((BeautyListPanel)beautyView).init(liveStreamManager);
        } else {
            beautyView.setBackgroundResource(R.color.livekit_design_standard_g1);
        }
        return beautyView;
    }
}
