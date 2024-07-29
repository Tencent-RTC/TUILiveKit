package com.trtc.uikit.livekit.view.voiceroom.view.preview;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;
import static android.view.ViewGroup.LayoutParams.WRAP_CONTENT;
import static com.trtc.uikit.livekit.common.utils.Constants.DATA_REPORT_COMPONENT_VOICE_ROOM;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_START_VOICE_ROOM;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;

import com.tencent.qcloud.tuicore.TUICore;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.preview.LiveInfoEditView;
import com.trtc.uikit.livekit.common.utils.Constants;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.view.voiceroom.view.seatview.SeatListView;

@SuppressLint("ViewConstructor")
public class AnchorPreviewView extends BasicView {

    public AnchorPreviewView(@NonNull Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void addObserver() {

    }

    @Override
    protected void removeObserver() {

    }

    @Override
    protected void initView() {
        View rootView = LayoutInflater.from(getContext()).inflate(R.layout.livekit_anchor_preview, this, true);
        initLiveInfoEditView(rootView);
        initFunctionView();
        initSeatListView();
        initListener();
    }

    private void initLiveInfoEditView(View view) {
        RelativeLayout liveStreamSettingsCardContainer = view.findViewById(R.id.rl_room_settings_card_container);
        LiveInfoEditView liveStreamSettingsCard = new LiveInfoEditView(mContext, mLiveController);
        liveStreamSettingsCardContainer.removeAllViews();
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        liveStreamSettingsCardContainer.addView(liveStreamSettingsCard, layoutParams);
    }

    private void initFunctionView() {
        RelativeLayout layoutFunctionViewContainer = findViewById(R.id.rl_function);
        layoutFunctionViewContainer.removeAllViews();
        AnchorPreviewFunctionView functionView = new AnchorPreviewFunctionView(mContext, mLiveController);
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        layoutFunctionViewContainer.addView(functionView, layoutParams);
    }

    private void initSeatListView() {
        RelativeLayout layoutSeatListContainer = findViewById(R.id.rl_preview_seat_list);
        ViewGroup.LayoutParams layoutParams = new ViewGroup.LayoutParams(MATCH_PARENT, WRAP_CONTENT);
        layoutSeatListContainer.removeAllViews();
        SeatListView.Config config = new SeatListView.Config();
        config.isPreview = true;
        SeatListView seatListView = new SeatListView(mContext, mLiveController, config);
        layoutSeatListContainer.addView(seatListView, layoutParams);
    }

    private void initListener() {
        findViewById(R.id.iv_back).setOnClickListener((view -> ((Activity) mContext).onBackPressed()));
        findViewById(R.id.btn_start_live).setOnClickListener((view) -> createRoom());
    }

    private void createRoom() {
        Constants.DATA_REPORT_COMPONENT = DATA_REPORT_COMPONENT_VOICE_ROOM;
        mLiveController.getRoomController().start();
        TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_START_VOICE_ROOM, null);
    }
}
