package com.trtc.uikit.livekit.manager;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.manager.controller.ConnectionController;
import com.trtc.uikit.livekit.manager.controller.MediaController;
import com.trtc.uikit.livekit.manager.controller.RoomController;
import com.trtc.uikit.livekit.manager.controller.SeatController;
import com.trtc.uikit.livekit.manager.controller.UserController;
import com.trtc.uikit.livekit.manager.controller.ViewController;
import com.trtc.uikit.livekit.manager.observer.LiveConnectionManagerObserver;
import com.trtc.uikit.livekit.manager.observer.LiveListManagerObserver;
import com.trtc.uikit.livekit.manager.observer.RoomEngineObserver;
import com.trtc.uikit.livekit.service.ILiveService;
import com.trtc.uikit.livekit.service.ServiceProvider;
import com.trtc.uikit.livekit.state.LiveState;
import com.trtc.uikit.livekit.state.operation.BeautyState;
import com.trtc.uikit.livekit.state.operation.ConnectionState;
import com.trtc.uikit.livekit.state.operation.MediaState;
import com.trtc.uikit.livekit.state.operation.RoomState;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.state.operation.UserState;
import com.trtc.uikit.livekit.state.view.ViewState;
import com.trtc.uikit.livekit.view.liveroom.view.common.video.VideoViewFactory;

public class LiveController {
    private final String                        mTag = "LiveController[" + hashCode() + "]";
    private final RoomController                mRoomController;
    private final SeatController                mSeatController;
    private final UserController                mUserController;
    private final MediaController               mMediaController;
    private final ViewController                mViewController;
    private final ConnectionController          mConnectionController;
    private final LiveState                     mState;
    private final ILiveService                  mLiveService;
    private final RoomEngineObserver            mRoomEngineObserver;
    private final LiveListManagerObserver       mLiveListManagerObserver;
    private final LiveConnectionManagerObserver mliveConnectionManagerObserver;
    private final VideoViewFactory              mVideoViewFactory;

    public LiveController() {
        mState = new LiveState();

        mLiveService = ServiceProvider.getInstance().getLiveService();
        mRoomController = new RoomController(mState, mLiveService);
        mSeatController = new SeatController(mState, mLiveService);
        mUserController = new UserController(mState, mLiveService);
        mMediaController = new MediaController(mState, mLiveService);
        mViewController = new ViewController(mState, mLiveService);
        mConnectionController = new ConnectionController(mState, mLiveService);
        mVideoViewFactory = new VideoViewFactory();
        mRoomEngineObserver = new RoomEngineObserver(this);
        mLiveListManagerObserver = new LiveListManagerObserver(this);
        mliveConnectionManagerObserver = new LiveConnectionManagerObserver(this);
        mLiveService.addRoomEngineObserver(mRoomEngineObserver);
        mLiveService.addLiveListManagerObserver(mLiveListManagerObserver);
        mLiveService.addLiveConnectionManagerObserver(mliveConnectionManagerObserver);
        mRoomController.addListener(mRoomControllerListener);
    }

    public void destroy() {
        destroyWithoutLiveService();
        mLiveService.destroy();
    }

    public void destroyWithoutLiveService() {
        mRoomController.removeListener(mRoomControllerListener);
        mLiveService.removeRoomEngineObserver(mRoomEngineObserver);
        mLiveService.removeLiveListManagerObserver(mLiveListManagerObserver);
        mLiveService.removeLiveConnectionManagerObserver(mliveConnectionManagerObserver);
        mRoomController.destroy();
        mSeatController.destroy();
        mUserController.destroy();
        mMediaController.destroy();
        mViewController.destroy();
        mConnectionController.destroy();
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

    public ConnectionController getConnectionController() {
        return mConnectionController;
    }

    public LiveState getState() {
        return mState;
    }

    public RoomState getRoomState() {
        return mState.operationState.roomState;
    }

    public SeatState getSeatState() {
        return mState.operationState.seatState;
    }

    public ConnectionState getConnectionState() {
        return mState.operationState.connectionState;
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
        getRoomState().roomId = roomId;
        LiveKitLog.info(mTag + " setRoomId:[mRoomId=" + roomId + ",mLiveService:" + mLiveService.hashCode()
                + ",mLiveObserver:" + mRoomEngineObserver.hashCode() + "]");
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
