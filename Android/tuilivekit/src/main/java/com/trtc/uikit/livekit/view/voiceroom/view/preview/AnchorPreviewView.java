package com.trtc.uikit.livekit.view.voiceroom.view.preview;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;
import static com.trtc.uikit.livekit.common.utils.Constants.DATA_REPORT_COMPONENT_VOICE_ROOM;
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
import com.trtc.uikit.livekit.common.uicomponent.preview.LiveInfoEditView;
import com.trtc.uikit.livekit.common.utils.Constants;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;

@SuppressLint("ViewConstructor")
public class AnchorPreviewView extends BasicView {
    private LiveInfoEditView mLiveStreamSettingsCard;

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
        View rootView = LayoutInflater.from(getContext()).inflate(R.layout.livekit_anchor_preview,
                this, true);
        initLiveInfoEditView(rootView);
        initListener();
    }

    private void initLiveInfoEditView(View view) {
        RelativeLayout liveStreamSettingsCardContainer = view.findViewById(R.id.rl_room_settings_card_container);
        mLiveStreamSettingsCard = new LiveInfoEditView(mContext, mLiveController);
        liveStreamSettingsCardContainer.removeAllViews();
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        liveStreamSettingsCardContainer.addView(mLiveStreamSettingsCard, layoutParams);
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
