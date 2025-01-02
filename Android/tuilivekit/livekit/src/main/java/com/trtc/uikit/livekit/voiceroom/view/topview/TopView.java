package com.trtc.uikit.livekit.voiceroom.view.topview;

import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.qcloud.tuicore.TUICore;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.component.audiencelist.AudienceListView;
import com.trtc.uikit.component.roominfo.RoomInfoView;
import com.trtc.uikit.livekit.voiceroom.api.Constants;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.view.BasicView;

import java.util.HashMap;
import java.util.Map;

public class TopView extends BasicView {
    private static boolean mIsRTCCube = false;

    public TopView(@NonNull Context context) {
        this(context, null);
    }

    public TopView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public TopView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public static void setIsRTCCube(boolean isRTCCube) {
        mIsRTCCube = isRTCCube;
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_top_view, this, true);
    }

    @Override
    public void init(@NonNull VoiceRoomManager voiceRoomManager) {
        super.init(voiceRoomManager);
        RoomInfoView roomInfoView = findViewById(R.id.rl_room_info);
        roomInfoView.init(mVoiceRoomManager.getRoomState().roomId);
        AudienceListView audienceListView = findViewById(R.id.rl_audience_list);
        audienceListView.init(mVoiceRoomManager.getRoomState().roomId);
        initReportView();
        initCloseView();
    }

    private void initReportView() {
        if (!TextUtils.equals(mUserState.selfInfo.userId, mRoomState.ownerInfo.userId)) {
            ImageView imageReport = findViewById(R.id.iv_report);
            imageReport.setVisibility(mIsRTCCube ? View.VISIBLE : View.GONE);
            imageReport.setOnClickListener(v -> {
                Map<String, Object> param = new HashMap<>();
                param.put("paramContext", getContext());
                param.put("paramRoomId", mRoomState.roomId);
                param.put("paramOwnerId", mRoomState.ownerInfo.userId);
                TUICore.callService("ReportViolatingService", "methodDisplayReportDialog", param);
            });
        }
    }

    @Override
    protected void addObserver() {
    }

    @Override
    protected void removeObserver() {
    }

    private void initCloseView() {
        ImageView mImageClose = findViewById(R.id.iv_close);
        mImageClose.setOnClickListener((view) -> TUICore.notifyEvent(Constants.EVENT_KEY_LIVE_KIT,
                Constants.EVENT_SUB_KEY_CLOSE_VOICE_ROOM, null));
    }
}
