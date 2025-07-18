package com.trtc.uikit.livekit.voiceroom.view.preview;

import android.app.Activity;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUIConstants;
import com.tencent.qcloud.tuicore.TUICore;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.state.RoomState;
import com.trtc.uikit.livekit.voiceroom.view.BasicView;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class AnchorPreviewView extends BasicView {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getVoiceRoomLogger("AnchorPreviewView");

    private boolean mIsExit = false;

    public AnchorPreviewView(@NonNull Context context) {
        this(context, null);
    }

    public AnchorPreviewView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AnchorPreviewView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected void addObserver() {

    }

    @Override
    protected void removeObserver() {

    }

    @Override
    protected void initView() {
        LayoutInflater.from(getContext()).inflate(R.layout.livekit_anchor_preview, this, true);
        findViewById(R.id.iv_back).setOnClickListener((view -> {
            mIsExit = true;
            ((Activity) mContext).finish();
        }));
        findViewById(R.id.btn_start_live).setOnClickListener((view) -> {
            if (view.isEnabled()) {
                view.setEnabled(false);
                createRoom();
            }
        });
    }

    @Override
    public void init(@NonNull VoiceRoomManager voiceRoomManager) {
        super.init(voiceRoomManager);
        initLiveInfoEditView();
        initFunctionView();
    }

    private void initLiveInfoEditView() {
        LiveInfoEditView liveStreamSettingsCard = findViewById(R.id.rl_live_info_edit_view);
        liveStreamSettingsCard.init(mVoiceRoomManager);
    }

    private void initFunctionView() {
        AnchorPreviewFunctionView functionView = findViewById(R.id.rl_function);
        functionView.init(mVoiceRoomManager, mSeatGridView);
    }

    private void createRoom() {
        TUIRoomDefine.RoomInfo roomInfo = new TUIRoomDefine.RoomInfo();
        roomInfo.roomType = TUIRoomDefine.RoomType.LIVE;
        roomInfo.isSeatEnabled = true;
        roomInfo.roomId = mVoiceRoomManager.getRoomState().roomId;
        roomInfo.name = mVoiceRoomManager.getRoomState().roomName.getValue();
        roomInfo.maxSeatCount = mVoiceRoomManager.getRoomState().maxSeatCount.getValue();
        roomInfo.seatMode = mVoiceRoomManager.getRoomState().seatMode.getValue();
        mSeatGridView.startVoiceRoom(roomInfo, new TUIRoomDefine.GetRoomInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                if (mIsExit) {
                    mSeatGridView.stopVoiceRoom(null);
                    return;
                }
                LOGGER.info("create room success");
                mVoiceRoomManager.getRoomManager().updateRoomState(roomInfo);
                mVoiceRoomManager.getRoomManager().updateLiveInfo();
                mVoiceRoomManager.getUserManager().getAudienceList();
                mVoiceRoomManager.getUserManager().updateOwnerUserInfo();
                mVoiceRoomManager.getSeatManager().getSeatList();
                mVoiceRoomManager.getRoomManager().updateLiveStatus(RoomState.LiveStatus.PUSHING);
                showAlertUserLiveTips();
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error(" create room failed, error: " + error + ", message: " + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    private void showAlertUserLiveTips() {
        try {
            Map<String, Object> map = new HashMap<>();
            map.put(TUIConstants.Privacy.PARAM_DIALOG_CONTEXT, Objects.requireNonNull(getContext()));
            TUICore.notifyEvent(TUIConstants.Privacy.EVENT_ROOM_STATE_CHANGED,
                    TUIConstants.Privacy.EVENT_SUB_KEY_ROOM_STATE_START, map);
        } catch (Exception e) {
            LOGGER.error("showAlertUserLiveTips exception:" + e.getMessage());
        }
    }
}
