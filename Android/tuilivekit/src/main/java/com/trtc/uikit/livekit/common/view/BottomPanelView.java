package com.trtc.uikit.livekit.common.view;

import android.content.Context;

import com.trtc.uikit.livekit.common.core.LiveController;

public abstract class BottomPanelView extends BasicView {
    protected OnBackButtonClickListener mOnBackButtonClickListener;

    public BottomPanelView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    protected void onBackButtonClick() {
        if (mOnBackButtonClickListener != null) {
            mOnBackButtonClickListener.onClick();
        }
    }

    public interface OnBackButtonClickListener {
        void onClick();
    }

    public void setOnBackButtonClickListener(OnBackButtonClickListener listener) {
        mOnBackButtonClickListener = listener;
    }
}
