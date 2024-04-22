package com.trtc.uikit.livekit.common.view;

import android.content.Context;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

import com.trtc.uikit.livekit.common.core.LiveController;
import com.trtc.uikit.livekit.common.core.store.state.operation.MediaState;
import com.trtc.uikit.livekit.common.core.store.state.operation.RoomState;
import com.trtc.uikit.livekit.common.core.store.state.operation.SeatState;
import com.trtc.uikit.livekit.common.core.store.state.operation.UserState;
import com.trtc.uikit.livekit.common.core.store.state.view.ViewState;

public abstract class BasicView extends FrameLayout {
    protected Context        mContext;
    protected LiveController mLiveController;
    protected RoomState      mRoomState;
    protected SeatState      mSeatState;
    protected UserState      mUserState;
    protected ViewState      mViewState;
    protected MediaState     mMediaState;

    public BasicView(@NonNull Context context, LiveController liveController) {
        super(context);
        mContext = context;
        mLiveController = liveController;
        mRoomState = liveController.getRoomSate();
        mSeatState = liveController.getSeatState();
        mUserState = liveController.getUserState();
        mMediaState = liveController.getMediaState();
        mViewState = liveController.getViewState();
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        addObserver();
        initView();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
        removeAllViews();
    }

    protected abstract void addObserver();

    protected abstract void removeObserver();

    protected abstract void initView();
}
