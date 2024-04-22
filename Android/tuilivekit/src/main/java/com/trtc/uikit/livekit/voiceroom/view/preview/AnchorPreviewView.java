package com.trtc.uikit.livekit.voiceroom.view.preview;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_START_VOICE_ROOM;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;

import com.tencent.qcloud.tuicore.TUICore;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveDefine;
import com.trtc.uikit.livekit.common.core.LiveController;
import com.trtc.uikit.livekit.common.core.store.state.operation.RoomState;
import com.trtc.uikit.livekit.common.view.BasicView;

@SuppressLint("ViewConstructor")
public class AnchorPreviewView extends BasicView {
    private final RoomState              mRoomState;
    private       LiveStreamSettingsCard mLiveStreamSettingsCard;

    public AnchorPreviewView(@NonNull Context context, LiveController liveController) {
        super(context, liveController);
        mRoomState = liveController.getRoomSate();
    }

    @Override
    protected void addObserver() {

    }

    @Override
    protected void removeObserver() {

    }

    @Override
    protected void initView() {
        View rootView = LayoutInflater.from(getContext()).inflate(R.layout.livekit_anchor_preview,
                this, true);
        initLiveInfoEditView(rootView);
        initListener();
    }

    private void initLiveInfoEditView(View view) {
        RelativeLayout liveStreamSettingsCardContainer = view.findViewById(R.id.rl_room_settings_card_container);
        mLiveStreamSettingsCard = new LiveStreamSettingsCard(mContext, mLiveController);
        liveStreamSettingsCardContainer.removeAllViews();
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        liveStreamSettingsCardContainer.addView(mLiveStreamSettingsCard, layoutParams);
    }

    private void initListener() {
        findViewById(R.id.iv_back).setOnClickListener((view -> ((Activity) mContext).onBackPressed()));
        findViewById(R.id.btn_start_live).setOnClickListener((view) -> createRoom());
    }

    private void createRoom() {
        LiveDefine.RoomParams params = new LiveDefine.RoomParams();
        params.maxSeatCount = mRoomState.maxSeatCount.get();
        params.seatMode = mRoomState.seatMode.get();
        String roomId = mRoomState.roomId;
        params.roomName = mLiveStreamSettingsCard.getRoomName();
        mLiveController.getRoomController().start(roomId, params);
        TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_START_VOICE_ROOM, null);
        mLiveController.getViewController().setShowAnchorPreview(false);
    }
}
