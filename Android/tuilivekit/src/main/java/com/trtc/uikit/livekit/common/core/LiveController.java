package com.trtc.uikit.livekit.common.core;

import com.trtc.uikit.livekit.common.core.controller.MediaController;
import com.trtc.uikit.livekit.common.core.controller.RoomController;
import com.trtc.uikit.livekit.common.core.controller.SeatController;
import com.trtc.uikit.livekit.common.core.controller.UserController;
import com.trtc.uikit.livekit.common.core.controller.ViewController;
import com.trtc.uikit.livekit.common.core.service.RoomEngineService;
import com.trtc.uikit.livekit.common.core.store.state.LiveState;
import com.trtc.uikit.livekit.common.core.store.state.operation.MediaState;
import com.trtc.uikit.livekit.common.core.store.state.operation.RoomState;
import com.trtc.uikit.livekit.common.core.store.state.operation.SeatState;
import com.trtc.uikit.livekit.common.core.store.state.operation.UserState;
import com.trtc.uikit.livekit.common.core.store.state.view.ViewState;

public class LiveController {
    private final RoomController  mRoomController;
    private final SeatController  mSeatController;
    private final UserController  mUserController;
    private final MediaController mMediaController;
    private final ViewController  mViewController;
    private final LiveState       mState;

    public LiveController() {
        mState = new LiveState();

        RoomEngineService roomEngineService = new RoomEngineService();
        mRoomController = new RoomController(mState, roomEngineService);
        mSeatController = new SeatController(mState, roomEngineService);
        mUserController = new UserController(mState, roomEngineService);
        mMediaController = new MediaController(mState, roomEngineService);
        mViewController = new ViewController(mState, roomEngineService);
    }

    public void destroy() {
        mRoomController.destroy();
        mSeatController.destroy();
        mUserController.destroy();
        mMediaController.destroy();
        mViewController.destroy();
    }

    public RoomController getRoomController() {
        return mRoomController;
    }

    public SeatController getSeatController() {
        return mSeatController;
    }

    public UserController getUserController() {
        return mUserController;
    }

    public MediaController getMediaController() {
        return mMediaController;
    }

    public ViewController getViewController() {
        return mViewController;
    }

    public LiveState getState() {
        return mState;
    }

    public RoomState getRoomSate() {
        return mState.operationState.roomState;
    }

    public SeatState getSeatState() {
        return mState.operationState.seatState;
    }

    public UserState getUserState() {
        return mState.operationState.userState;
    }

    public MediaState getMediaState() {
        return mState.operationState.mediaState;
    }

    public ViewState getViewState() {
        return mState.viewState;
    }
}
