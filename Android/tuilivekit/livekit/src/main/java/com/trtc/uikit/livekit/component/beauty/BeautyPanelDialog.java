package com.trtc.uikit.livekit.component.beauty;

import android.content.Context;
import android.view.View;

import androidx.annotation.NonNull;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.component.beauty.basicbeauty.BeautyListPanel;
import com.trtc.uikit.livekit.component.beauty.tebeauty.TEBeautyManager;
import com.trtc.uikit.livekit.component.beauty.tebeauty.TEBeautyView;

public class BeautyPanelDialog extends PopupDialog {

    public BeautyPanelDialog(@NonNull Context context) {
        super(context, com.trtc.tuikit.common.R.style.TUICommonBottomDialogTheme);
        View beautyView = null;
        if (TEBeautyManager.getInstance().isSupportTEBeauty()) {
            beautyView = new TEBeautyView(context);
        } else {
            beautyView = new BeautyListPanel(context);
        }
        setView(beautyView);
    }
}
