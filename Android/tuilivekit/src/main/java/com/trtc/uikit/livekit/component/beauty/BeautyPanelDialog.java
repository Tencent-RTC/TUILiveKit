package com.trtc.uikit.livekit.component.beauty;

import android.content.Context;
import android.os.Bundle;
import android.view.View;

import androidx.annotation.NonNull;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.component.beauty.basicbeauty.BeautyListPanel;
import com.trtc.uikit.livekit.component.beauty.tebeauty.TEBeautyManager;
import com.trtc.uikit.livekit.component.beauty.tebeauty.TEBeautyView;

public class BeautyPanelDialog extends PopupDialog {

    private final Context mContext;

    public BeautyPanelDialog(@NonNull Context context) {
        super(context, com.trtc.tuikit.common.R.style.TUICommonBottomDialogTheme);
        mContext = context;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        View beautyView = null;
        if (TEBeautyManager.getInstance().isSupportTEBeauty()) {
            beautyView = new TEBeautyView(mContext);
        } else {
            beautyView = new BeautyListPanel(mContext);
        }
        setView(beautyView);
        super.onCreate(savedInstanceState);
    }
}
