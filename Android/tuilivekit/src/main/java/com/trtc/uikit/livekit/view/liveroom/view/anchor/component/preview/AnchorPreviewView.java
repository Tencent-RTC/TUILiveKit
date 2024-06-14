package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.preview;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;
import static com.trtc.uikit.livekit.common.utils.Constants.DATA_REPORT_COMPONENT_LIVE_ROOM;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_START_LIVE_ROOM;

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
import com.trtc.uikit.livekit.state.LiveDefine;

@SuppressLint("ViewConstructor")
public class AnchorPreviewView extends BasicView {

    public AnchorPreviewView(@NonNull Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(getContext()).inflate(R.layout.livekit_anchor_preview, this, true);

        initLiveInfoEditView();
        initFunctionView();
        initBackView();
        initStartLiveView();
    }

    @Override
    protected void addObserver() {
    }

    @Override
    protected void removeObserver() {
    }

    private void initBackView() {
        findViewById(R.id.iv_back).setOnClickListener((view -> {
            if (mViewState.liveStatus.get() == LiveDefine.LiveStatus.PREVIEWING) {
                if (mContext instanceof Activity) {
                    ((Activity) mContext).onBackPressed();
                }
            }
        }));
    }

    private void initLiveInfoEditView() {
        RelativeLayout liveStreamSettingsCardContainer = findViewById(R.id.rl_room_settings_card_container);
        LiveInfoEditView mLiveInfoEditView = new LiveInfoEditView(mContext, mLiveController);
        liveStreamSettingsCardContainer.removeAllViews();
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        liveStreamSettingsCardContainer.addView(mLiveInfoEditView, layoutParams);
    }

    private void initFunctionView() {
        RelativeLayout layoutFunctionViewContainer = findViewById(R.id.rl_function);
        layoutFunctionViewContainer.removeAllViews();
        AnchorPreviewFunctionView functionView = new AnchorPreviewFunctionView(mContext, mLiveController);
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        layoutFunctionViewContainer.addView(functionView, layoutParams);
    }

    private void initStartLiveView() {
        findViewById(R.id.btn_start_live).setOnClickListener((View view) -> createRoom());
    }

    private void createRoom() {
        Constants.DATA_REPORT_COMPONENT = DATA_REPORT_COMPONENT_LIVE_ROOM;
        mLiveController.getRoomController().start();
        TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_START_LIVE_ROOM, null);
    }
}

