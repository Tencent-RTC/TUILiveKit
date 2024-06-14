package com.trtc.uikit.livekit.view.voiceroom.model;

import android.view.View;

import androidx.annotation.DrawableRes;

import com.trtc.uikit.livekit.state.LiveDefine;

public class BottomMenuInfo {
    public BottomMenuInfo(LiveDefine.NavigationStatus navigationState, int icon, OnClickListener listener) {
        this.icon = icon;
        this.navigationState = navigationState;
        this.listener = listener;
    }

    public LiveDefine.NavigationStatus navigationState;
    @DrawableRes
    public int                         icon;
    public OnClickListener             listener;
    public View                        view;

    public interface OnClickListener {
        void onClick();
    }
}
