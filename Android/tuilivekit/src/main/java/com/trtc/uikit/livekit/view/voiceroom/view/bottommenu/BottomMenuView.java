package com.trtc.uikit.livekit.view.voiceroom.view.bottommenu;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.barrage.TUIBarrageButton;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.LiveDefine;

@SuppressLint("ViewConstructor")
public class BottomMenuView extends BasicView {
    private RelativeLayout mBarrageButtonContainer;
    private View           mMicrophoneContainer;
    private ImageView      mMicrophoneButton;

    private final Observer<LiveDefine.LinkStatus> mLinkStateObserver = this::onLinkStateChanged;

    private final Observer<Boolean> mMicrophoneMutedObserver = this::updateMicrophoneButton;

    public BottomMenuView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_layout_bottom_menu, this, true);
        mBarrageButtonContainer = findViewById(R.id.rl_barrage_button);
        mMicrophoneContainer = findViewById(R.id.microphone_container);
        mMicrophoneButton = findViewById(R.id.iv_microphone);
        mMicrophoneButton.setOnClickListener(v -> onMicrophoneButtonClick());
        showAudienceBarrageSendButton();
        initFunctionContainer();
    }

    @Override
    protected void addObserver() {
        mViewState.linkStatus.observe(mLinkStateObserver);
        mMediaState.isMicrophoneMuted.observe(mMicrophoneMutedObserver);
    }

    @Override
    protected void removeObserver() {
        mViewState.linkStatus.removeObserver(mLinkStateObserver);
        mMediaState.isMicrophoneMuted.removeObserver(mMicrophoneMutedObserver);
    }

    private void initFunctionContainer() {
        View functionView;
        if (mUserState.selfInfo.role.get() == TUIRoomDefine.Role.ROOM_OWNER) {
            functionView = new AnchorFunctionView(mContext, mLiveController);
        } else {
            functionView = new AudienceFunctionView(mContext, mLiveController);
        }
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        RelativeLayout functionContainer = findViewById(R.id.function_container);
        functionContainer.removeAllViews();
        functionContainer.addView(functionView, layoutParams);
    }

    private void onLinkStateChanged(LiveDefine.LinkStatus linkStatus) {
        mMicrophoneContainer.setVisibility(linkStatus == LiveDefine.LinkStatus.LINKING ? VISIBLE : INVISIBLE);
    }

    private void showAudienceBarrageSendButton() {
        TUIBarrageButton barrageButton = new TUIBarrageButton(mContext, mRoomState.roomId, mRoomState.ownerInfo.userId);
        mBarrageButtonContainer.addView(barrageButton);
    }

    private void onMicrophoneButtonClick() {
        mMediaController.operateMicrophone();
    }

    private void updateMicrophoneButton(boolean isMicrophoneMuted) {
        mMicrophoneButton.setImageResource(isMicrophoneMuted ? R.drawable.livekit_ic_mic_closed :
                R.drawable.livekit_ic_mic_opened);
    }
}

