package com.trtc.uikit.livekit.voiceroomcore.manager.module;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.voiceroomcore.manager.observer.SeatGridViewObserverManager;
import com.trtc.uikit.livekit.voiceroomcore.manager.api.IVoiceRoomService;
import com.trtc.uikit.livekit.voiceroomcore.state.MediaState;
import com.trtc.uikit.livekit.voiceroomcore.state.RoomState;
import com.trtc.uikit.livekit.voiceroomcore.state.SeatState;
import com.trtc.uikit.livekit.voiceroomcore.state.UserState;
import com.trtc.uikit.livekit.voiceroomcore.state.ViewState;
import com.trtc.uikit.livekit.voiceroomcore.state.VoiceRoomState;

public abstract class BaseManager {
    protected VoiceRoomState              mState;
    protected IVoiceRoomService           mService;
    protected SeatGridViewObserverManager mSeatGridViewObserverManager;

    protected RoomState  mRoomState;
    protected SeatState  mSeatState;
    protected UserState  mUserState;
    protected MediaState mMediaState;
    protected ViewState  mViewState;

    protected BaseManager(VoiceRoomState state, IVoiceRoomService service,
                          SeatGridViewObserverManager observerManager) {
        mState = state;
        mService = service;
        mSeatGridViewObserverManager = observerManager;
        mRoomState = mState.roomState;
        mSeatState = mState.seatState;
        mUserState = mState.userState;
        mMediaState = mState.mediaState;
        mViewState = mState.viewState;
    }

    public abstract void destroy();

    protected void getUserInfo(String userId, GetLiveUserCallback callback) {
        mService.getUserInfo(userId, new TUIRoomDefine.GetUserInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserInfo userInfo) {
                callback.onComplete(userInfo);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {
                TUIRoomDefine.UserInfo userInfo = new TUIRoomDefine.UserInfo();
                userInfo.userId = userId;
                callback.onComplete(userInfo);
            }
        });
    }

    public interface GetLiveUserCallback {
        void onComplete(TUIRoomDefine.UserInfo user);
    }
}
