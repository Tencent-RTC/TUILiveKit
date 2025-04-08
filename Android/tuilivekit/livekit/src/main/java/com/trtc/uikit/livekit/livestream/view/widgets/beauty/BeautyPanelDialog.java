package com.trtc.uikit.livekit.livestream.view.widgets.beauty;

import android.content.Context;
import android.view.View;

import androidx.annotation.NonNull;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;

public class BeautyPanelDialog extends PopupDialog {

    public BeautyPanelDialog(@NonNull Context context, LiveStreamManager liveStreamManager) {
        super(context, com.trtc.tuikit.common.R.style.TUICommonBottomDialogTheme);
        View beautyView = null;
        if (TEBeautyManager.getInstance().isSupportTEBeauty()) {
            beautyView = new TEBeautyView(getContext());
            beautyView.setBackgroundResource(R.color.common_design_standard_g1);
        } else {
            beautyView = new BeautyListPanel(getContext());
            ((BeautyListPanel) beautyView).init(liveStreamManager);
        }
        setView(beautyView);
    }
}
