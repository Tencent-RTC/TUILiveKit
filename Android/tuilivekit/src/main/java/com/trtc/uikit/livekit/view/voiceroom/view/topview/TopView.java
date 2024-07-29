package com.trtc.uikit.livekit.view.voiceroom.view.topview;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;
import static android.view.ViewGroup.LayoutParams.WRAP_CONTENT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_CLOSE_VOICE_ROOM;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;

import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.audiencelist.AudienceListView;
import com.trtc.uikit.livekit.common.uicomponent.roominfo.RoomInfoView;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.LiveDefine;

import java.util.Map;

@SuppressLint("ViewConstructor")
public class TopView extends BasicView implements ITUINotification {
    public TopView(@NonNull Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_top_view, this, true);
        initCloseView();
        initRoomInfoView();
        initAudienceListView();
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_CLOSE_VOICE_ROOM, this);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        TUICore.unRegisterEvent(this);
    }

    @Override
    public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {
        if (EVENT_SUB_KEY_CLOSE_VOICE_ROOM.equals(subKey)) {
            closeVoiceRoom();
        }
    }

    private void initRoomInfoView() {
        RelativeLayout layoutRoomInfoContainer = findViewById(R.id.rl_room_info);
        layoutRoomInfoContainer.removeAllViews();
        RoomInfoView roomInfoView = new RoomInfoView(mContext, mLiveController);
        layoutRoomInfoContainer.addView(roomInfoView);
    }

    private void initAudienceListView() {
        RelativeLayout layoutAudienceListContainer = findViewById(R.id.rl_audience_list);
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(WRAP_CONTENT, MATCH_PARENT);
        layoutAudienceListContainer.removeAllViews();
        AudienceListView mAudienceListView = new AudienceListView(mContext, mLiveController);
        layoutAudienceListContainer.addView(mAudienceListView, layoutParams);
    }

    @Override
    protected void addObserver() {

    }

    @Override
    protected void removeObserver() {

    }

    private void initCloseView() {
        ImageView mImageClose = findViewById(R.id.iv_close);
        mImageClose.setOnClickListener((view) -> closeVoiceRoom());
    }

    private void closeVoiceRoom() {
        mLiveController.getRoomController().exit();
        if (mLiveController.getRoomController().isOwner()) {
            mViewController.updateLiveStatus(LiveDefine.LiveStatus.DASHBOARD);
        } else {
            if (mContext instanceof Activity) {
                ((Activity) mContext).finish();
            }
        }
    }
}
