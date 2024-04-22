package com.trtc.uikit.livekit.common.core.model;

import android.view.View;

import androidx.annotation.DrawableRes;

import com.trtc.uikit.livekit.common.core.store.state.view.ViewState;

public class BottomMenuInfo {
    public BottomMenuInfo(ViewState.NavigationState navigationState, int icon, OnClickListener listener) {
        this.icon = icon;
        this.navigationState = navigationState;
        this.listener = listener;
    }

    public ViewState.NavigationState navigationState;
    @DrawableRes
    public int                       icon;
    public OnClickListener           listener;
    public View                      view;

    public interface OnClickListener {
        void onClick();
    }
}
