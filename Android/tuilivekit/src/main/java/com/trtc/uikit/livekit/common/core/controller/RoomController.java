package com.trtc.uikit.livekit.common.core.controller;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveDefine;
import com.trtc.uikit.livekit.common.core.error.ErrorHandler;
import com.trtc.uikit.livekit.common.core.service.RoomEngineService;
import com.trtc.uikit.livekit.common.core.store.state.LiveState;
import com.trtc.uikit.livekit.common.core.store.state.operation.RoomState;
import com.trtc.uikit.livekit.common.core.store.state.view.ViewState;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.common.utils.ToastUtils;

public class RoomController extends Controller {
    private static final String TAG = "RoomController";

    public RoomController(LiveState state, RoomEngineService service) {
        super(state, service);
        mRoomEngineService.addObserver(mObserver);
    }

    @Override
    public void destroy() {
        mRoomEngineService.removeObserver(mObserver);
    }

    public void setRoomParams(String roomId, LiveDefine.RoomParams params) {
        mRoomState.roomId = roomId;
        mRoomState.roomName.set(params.roomName);
        mRoomState.seatMode.set(params.seatMode);
        mRoomState.maxSeatCount.set(params.maxSeatCount);
    }

    public void start(String roomId, LiveDefine.RoomParams params) {
        TUIRoomDefine.RoomInfo roomInfo = new TUIRoomDefine.RoomInfo();
        roomInfo.roomId = roomId;
        roomInfo.name = params.roomName;
        roomInfo.roomType = TUIRoomDefine.RoomType.LIVE;
        roomInfo.maxSeatCount = params.maxSeatCount;
        roomInfo.isSeatEnabled = true;
        roomInfo.seatMode = params.seatMode;
        mRoomEngineService.start(roomInfo, new TUIRoomDefine.GetRoomInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                LiveKitLog.info(TAG + "[" + roomInfo.roomId + "] start:[Success]");
                onEnterRoomSuccess(roomInfo);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(TAG + "[" + roomInfo.roomId + "] start:[Error] error:"
                        + error + ",message:" + message + "]");
                ErrorHandler.onError(error);
                mViewState.currentNavigationState.set(ViewState.NavigationState.EXIT);
            }
        });
    }

    public void join(String roomId) {
        mRoomEngineService.join(roomId, new TUIRoomDefine.GetRoomInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                LiveKitLog.info(TAG + "[" + roomInfo.roomId + "] join:[Success]");
                onEnterRoomSuccess(roomInfo);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(TAG + "[" + roomId + "] enter:[Error] error:"
                        + error + ",message:" + message + "]");
                ErrorHandler.onError(error);
                mViewState.currentNavigationState.set(ViewState.NavigationState.EXIT);
            }
        });
    }

    public void exit() {
        LiveKitLog.info(TAG + " exit start");
        if (isOwner()) {
            stop();
        } else {
            leave();
        }
        mRoomState.enterRoomState.set(RoomState.EnterRoomSate.NOT_ENTERED);
    }

    public void updateCreateTime(long time) {
        mRoomState.createTime = time;
    }

    public void updateMaxAudienceNumber(int maxCount) {
        mRoomState.maxAudienceNumber = maxCount;
    }

    public void updateMessageCount(int messageCount) {
        mRoomState.messageCount = messageCount;
    }

    public void updateGiftIncome(int giftIncome) {
        mRoomState.giftIncome = giftIncome;
    }

    public void insertGiftPeople(String userId) {
        mRoomState.giftPeopleSet.add(userId);
    }

    public void updateLikeNumber(int messageCount) {
        mRoomState.likeNumber = messageCount;
    }

    private void leave() {
        mRoomEngineService.leave(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(TAG + " leave success");
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(TAG + " leave:[Error] error:" + error + ",message:" + message + "]");
            }
        });
    }

    private void stop() {
        mRoomEngineService.stop(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(TAG + " stop success");
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(TAG + " stop:[Error] error:" + error + ",message:" + message + "]");
            }
        });
    }

    private boolean isOwner() {
        String selfUserId = TUIRoomEngine.getSelfInfo().userId;
        if (TextUtils.isEmpty(selfUserId)) {
            return false;
        }
        return selfUserId.equals(mRoomState.ownerId.get());
    }

    private void onEnterRoomSuccess(TUIRoomDefine.RoomInfo roomInfo) {
        updateRoomState(roomInfo);
        updateSelfRole();
        mRoomState.enterRoomState.set(RoomState.EnterRoomSate.IN_ROOM);
    }

    private void updateRoomState(TUIRoomDefine.RoomInfo roomInfo) {
        mRoomState.updateState(roomInfo);
    }

    private void updateSelfRole() {
        if (isOwner()) {
            mUserState.selfInfo.role.set(TUIRoomDefine.Role.ROOM_OWNER);
        } else {
            mUserState.selfInfo.role.set(TUIRoomDefine.Role.GENERAL_USER);
        }
    }

    private final TUIRoomObserver mObserver = new TUIRoomObserver() {
        @Override
        public void onRoomDismissed(String roomId) {
            LiveKitLog.info(TAG + " onRoomDismissed : " + roomId);
            ToastUtils.toast(R.string.livekit_room_destroy);
            mViewState.showEndView.set(true);
            mRoomState.enterRoomState.set(RoomState.EnterRoomSate.NOT_ENTERED);
        }

        @Override
        public void onRoomUserCountChanged(String roomId, int userCount) {
            LiveKitLog.info(TAG + " onRoomUserCountChanged : " + userCount);
            if (userCount > 0) {
                mRoomState.audienceCount.set(userCount - 1);
                if (userCount > mRoomState.maxAudienceNumber) {
                    mRoomState.maxAudienceNumber = userCount - 1;
                }
            }
        }
    };
}
