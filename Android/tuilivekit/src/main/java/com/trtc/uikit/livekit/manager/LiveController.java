package com.trtc.uikit.livekit.manager;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.manager.controller.MediaController;
import com.trtc.uikit.livekit.manager.controller.RoomController;
import com.trtc.uikit.livekit.manager.controller.SeatController;
import com.trtc.uikit.livekit.manager.controller.UserController;
import com.trtc.uikit.livekit.manager.controller.ViewController;
import com.trtc.uikit.livekit.manager.observer.LiveObserver;
import com.trtc.uikit.livekit.service.ILiveService;
import com.trtc.uikit.livekit.service.ServiceProvider;
import com.trtc.uikit.livekit.state.LiveState;
import com.trtc.uikit.livekit.state.operation.BeautyState;
import com.trtc.uikit.livekit.state.operation.MediaState;
import com.trtc.uikit.livekit.state.operation.RoomState;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.state.operation.UserState;
import com.trtc.uikit.livekit.state.view.ViewState;
import com.trtc.uikit.livekit.view.liveroom.view.common.video.VideoViewFactory;

public class LiveController {
    private final String           mTag = "LiveController[" + hashCode() + "]";
    private final RoomController   mRoomController;
    private final SeatController   mSeatController;
    private final UserController   mUserController;
    private final MediaController  mMediaController;
    private final ViewController   mViewController;
    private final LiveState        mState;
    private final ILiveService     mLiveService;
    private final LiveObserver     mLiveObserver;
    private final VideoViewFactory mVideoViewFactory;

    public LiveController() {
        mState = new LiveState();

        mLiveService = ServiceProvider.getInstance().getLiveService();
        mRoomController = new RoomController(mState, mLiveService);
        mSeatController = new SeatController(mState, mLiveService);
        mUserController = new UserController(mState, mLiveService);
        mMediaController = new MediaController(mState, mLiveService);
        mViewController = new ViewController(mState, mLiveService);
        mVideoViewFactory = new VideoViewFactory();
        mLiveObserver = new LiveObserver(this);
        mLiveService.addObserver(mLiveObserver);
        mRoomController.addListener(mRoomControllerListener);
    }

    public void destroy() {
        mRoomController.removeListener(mRoomControllerListener);
        mLiveService.removeObserver(mLiveObserver);
        mRoomController.destroy();
        mSeatController.destroy();
        mUserController.destroy();
        mMediaController.destroy();
        mViewController.destroy();
        mLiveService.destroy();
    }

    public ILiveService getLiveService() {
        return mLiveService;
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

    public BeautyState getBeautyState() {
        return mState.operationState.beautyState;
    }

    public ViewState getViewState() {
        return mState.viewState;
    }

    public VideoViewFactory getVideoViewFactory() {
        return mVideoViewFactory;
    }

    public void setRoomId(String roomId) {
        getRoomSate().roomId = roomId;
        LiveKitLog.info(mTag + " setRoomId:[mRoomId=" + roomId + ",mLiveService:" + mLiveService.hashCode()
                + ",mLiveObserver:" + mLiveObserver.hashCode() + "]");
    }

    private final RoomController.Listener mRoomControllerListener = new RoomController.Listener() {
        @Override
        public void onEnterRoomSuccess() {
            mUserController.getAudienceList();
            mUserController.updateOwnerUserInfo();
            mSeatController.getSeatList();
            if (mState.operationState.userState.selfInfo.role.get() == TUIRoomDefine.Role.ROOM_OWNER) {
                mSeatController.getSeatApplicationList();
            }
        }
    };
}
