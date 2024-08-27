package com.trtc.uikit.livekit.common.view;

import android.content.Context;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.manager.controller.ConnectionController;
import com.trtc.uikit.livekit.manager.controller.MediaController;
import com.trtc.uikit.livekit.manager.controller.SeatController;
import com.trtc.uikit.livekit.manager.controller.UserController;
import com.trtc.uikit.livekit.manager.controller.ViewController;
import com.trtc.uikit.livekit.state.operation.BeautyState;
import com.trtc.uikit.livekit.state.operation.ConnectionState;
import com.trtc.uikit.livekit.state.operation.MediaState;
import com.trtc.uikit.livekit.state.operation.RoomState;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.state.operation.UserState;
import com.trtc.uikit.livekit.state.view.ViewState;

public abstract class BasicView extends FrameLayout {
    protected       Context              mContext;
    protected final LiveController       mLiveController;
    protected final SeatController       mSeatController;
    protected final ConnectionController mConnectionController;
    protected final UserController       mUserController;
    protected final MediaController      mMediaController;
    protected final ViewController       mViewController;

    protected RoomState       mRoomState;
    protected SeatState       mSeatState;
    protected ConnectionState mConnectionState;
    protected UserState       mUserState;
    protected ViewState       mViewState;
    protected MediaState      mMediaState;
    protected BeautyState     mBeautyState;

    public BasicView(@NonNull Context context, @NonNull LiveController liveController) {
        super(context);
        mContext = context;
        mLiveController = liveController;
        mSeatController = liveController.getSeatController();
        mUserController = liveController.getUserController();
        mMediaController = liveController.getMediaController();
        mViewController = liveController.getViewController();
        mConnectionController = liveController.getConnectionController();
        mRoomState = liveController.getRoomState();
        mSeatState = liveController.getSeatState();
        mUserState = liveController.getUserState();
        mMediaState = liveController.getMediaState();
        mBeautyState = liveController.getBeautyState();
        mViewState = liveController.getViewState();
        mConnectionState = liveController.getConnectionState();
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        initView();
        addObserver();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
        removeAllViews();
    }

    protected abstract void initView();

    protected abstract void addObserver();

    protected abstract void removeObserver();

}
