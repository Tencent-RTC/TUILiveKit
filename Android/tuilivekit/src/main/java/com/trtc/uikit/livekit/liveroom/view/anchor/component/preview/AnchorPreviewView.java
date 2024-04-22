package com.trtc.uikit.livekit.liveroom.view.anchor.component.preview;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_START_LIVE_ROOM;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;
import androidx.constraintlayout.widget.ConstraintLayout;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.core.TUILiveDefine;
import com.trtc.uikit.livekit.liveroom.core.UserManager;
import com.trtc.uikit.livekit.liveroom.core.listener.GetUserInfoCallback;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.data.UserInfo;

@SuppressLint("ViewConstructor")
public class AnchorPreviewView extends ConstraintLayout {
    private static final String TAG = "AnchorPreviewView";

    private final Context                mContext;
    private final LiveRoomInfo           mLiveRoomInfo;
    private final RoomEngineService      mRoomEngineService;
    private       LiveStreamSettingsCard mLiveStreamSettingsCard;

    public AnchorPreviewView(@NonNull Context context, LiveRoomInfo roomInfo, RoomEngineService service) {
        super(context, null);
        mContext = context;
        mLiveRoomInfo = roomInfo;
        mRoomEngineService = service;
        init();
    }

    private void init() {
        View rootView = LayoutInflater.from(getContext()).inflate(R.layout.livekit_anchor_preview,
                this, true);

        initLiveInfoEditView(rootView);
        initFunctionView(rootView);
        initListener();
    }

    private void initLiveInfoEditView(View view) {
        RelativeLayout liveStreamSettingsCardContainer = view.findViewById(R.id.rl_room_settings_card_container);
        mLiveStreamSettingsCard = new LiveStreamSettingsCard(mContext, mLiveRoomInfo);
        liveStreamSettingsCardContainer.removeAllViews();
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        liveStreamSettingsCardContainer.addView(mLiveStreamSettingsCard, layoutParams);
    }

    private void initFunctionView(View view) {
        RelativeLayout layoutFunctionViewContainer = view.findViewById(R.id.rl_function);
        layoutFunctionViewContainer.removeAllViews();
        FunctionView functionView = new FunctionView(mContext, mLiveRoomInfo, mRoomEngineService);
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        layoutFunctionViewContainer.addView(functionView, layoutParams);
    }

    private void initListener() {
        findViewById(R.id.iv_back).setOnClickListener((view -> {
            if (mLiveRoomInfo.userLiveStatus.get() == TUILiveDefine.UserLiveStatus.PREVIEWING) {
                if (mContext instanceof Activity) {
                    ((Activity) mContext).onBackPressed();
                }
            }
        }));

        findViewById(R.id.btn_start_live).setOnClickListener((view) -> {
            mLiveRoomInfo.name.set(mLiveStreamSettingsCard.getStreamId());
            createRoom();
        });
    }

    private void createRoom() {
        mLiveRoomInfo.userLiveStatus.set(TUILiveDefine.UserLiveStatus.PUSHING);
        TUIRoomDefine.RoomInfo roomInfo = new TUIRoomDefine.RoomInfo();
        roomInfo.roomId = mLiveRoomInfo.roomId;
        roomInfo.ownerId = TUILogin.getUserId();
        roomInfo.name = mLiveRoomInfo.name.get();
        roomInfo.roomType = TUIRoomDefine.RoomType.LIVE;
        roomInfo.seatMode = TUIRoomDefine.SeatMode.APPLY_TO_TAKE;
        roomInfo.isSeatEnabled = true;
        roomInfo.maxSeatCount = 9;
        TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_START_LIVE_ROOM, null);
        mRoomEngineService.createRoom(roomInfo, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mRoomEngineService.enterRoom(mLiveRoomInfo.roomId, new TUIRoomDefine.GetRoomInfoCallback() {
                    @Override
                    public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                        setSelfInfo(roomInfo);
                        mRoomEngineService.enableLinkMicRequest(true);
                        mRoomEngineService.takeSeat(-1, 0, new TUIRoomDefine.RequestCallback() {
                            @Override
                            public void onAccepted(String requestId, String userId) {
                                mRoomEngineService.openLocalMicrophone();
                            }

                            @Override
                            public void onRejected(String requestId, String userId, String message) {

                            }

                            @Override
                            public void onCancelled(String requestId, String userId) {

                            }

                            @Override
                            public void onTimeout(String requestId, String userId) {

                            }

                            @Override
                            public void onError(String requestId, String userId
                                    , TUICommonDefine.Error error, String message) {

                            }
                        });
                    }

                    @Override
                    public void onError(TUICommonDefine.Error error, String message) {
                        Log.i(TAG, "enterRoom onError:" + message);
                        ToastUtil.toastShortMessage(message);
                    }
                });

            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Log.i(TAG, "enterRoom onError:" + message);
                ToastUtil.toastShortMessage(message);
            }
        });
    }

    private void setSelfInfo(TUIRoomDefine.RoomInfo roomInfo) {
        UserManager.getInstance().getUserInfo(roomInfo.ownerId, new GetUserInfoCallback() {
            @Override
            public void onSuccess(UserInfo userInfo) {
                mLiveRoomInfo.anchorInfo.name.set(userInfo.name.get());
                mLiveRoomInfo.anchorInfo.avatarUrl.set(userInfo.avatarUrl.get());
                mLiveRoomInfo.anchorInfo.role.set(TUILiveDefine.RoleType.ANCHOR);
            }

            @Override
            public void onError(int errorCode, String errorMessage) {
            }
        });
    }
}

