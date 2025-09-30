package com.trtc.uikit.livekit.voiceroom.view.topview;

import static com.trtc.uikit.livekit.common.ConstantsKt.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.common.ConstantsKt.EVENT_SUB_KEY_CLOSE_VOICE_ROOM;

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
import com.trtc.uikit.livekit.component.audiencelist.AudienceListView;
import com.trtc.uikit.livekit.component.roominfo.LiveInfoView;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.view.BasicView;

import java.util.HashMap;
import java.util.Map;

public class TopView extends BasicView {
    private static boolean mIsRTCCube = false;

    private LiveInfoView mLiveInfoView;

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
        mLiveInfoView = findViewById(R.id.rl_room_info);
    }

    @Override
    public void init(@NonNull VoiceRoomManager voiceRoomManager) {
        super.init(voiceRoomManager);
        AudienceListView audienceListView = findViewById(R.id.rl_audience_list);
        audienceListView.init(mVoiceRoomManager.getRoomState().liveInfo);
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
        mLiveInfoView.init(mVoiceRoomManager.getRoomState().liveInfo);
    }

    @Override
    protected void removeObserver() {
        mLiveInfoView.unInit();
    }

    private void initCloseView() {
        ImageView mImageClose = findViewById(R.id.iv_close);
        mImageClose.setOnClickListener((view) -> TUICore.notifyEvent(EVENT_KEY_LIVE_KIT,
                EVENT_SUB_KEY_CLOSE_VOICE_ROOM, null));
    }
}
