package com.trtc.uikit.livekit.liveroom.view.anchor.component.preview;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.LinearLayout;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.store.LiveStore;
import com.trtc.uikit.livekit.common.uicomponent.audioeffect.AudioEffectPanel;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.view.anchor.component.common.BeautyListPanel;

@SuppressLint("ViewConstructor")
public class FunctionView extends LinearLayout {

    private final Context           mContext;
    private final LiveRoomInfo      mLiveRoomInfo;
    private final RoomEngineService mRoomEngineService;
    private       AudioEffectPanel  mAudioEffectPanel;
    private       PopupDialog       mDialogBeautyList;
    private       BeautyListPanel   mBeautyListPanel;


    public FunctionView(Context context, LiveRoomInfo roomInfo, RoomEngineService roomEngineService) {
        super(context);
        mContext = context;
        mLiveRoomInfo = roomInfo;
        mRoomEngineService = roomEngineService;
        init();
    }

    private void init() {
        LayoutInflater.from(getContext()).inflate(R.layout.livekit_layout_anchor_preview_function, this,
                true);
        initBeautyButton();
        initAudioEffectButton();
        initFlipButton();
        initMirrorButton();
    }

    private void initBeautyButton() {
        findViewById(R.id.iv_beauty).setOnClickListener(view -> {
            if (mDialogBeautyList == null) {
                mDialogBeautyList = new PopupDialog(mContext,
                        com.trtc.tuikit.common.R.style.TUICommonBottomDialogTheme);
                mDialogBeautyList.setOnDismissListener((dialogInterface) -> {
                });
            }
            if (mBeautyListPanel == null) {
                mBeautyListPanel = new BeautyListPanel(mContext, mLiveRoomInfo, mRoomEngineService);
            }
            mDialogBeautyList.setView(mBeautyListPanel);
            mDialogBeautyList.show();
        });
    }

    private void initAudioEffectButton() {
        findViewById(R.id.iv_audio_effect).setOnClickListener(view -> {
            if (mAudioEffectPanel == null) {
                mAudioEffectPanel = new AudioEffectPanel(mContext, LiveStore.sharedInstance().getLiveController());
            }
            mAudioEffectPanel.show();
        });
    }

    private void initFlipButton() {
        findViewById(R.id.iv_flip).setOnClickListener(view -> {
            mRoomEngineService.switchCamera();
        });
    }

    private void initMirrorButton() {
        findViewById(R.id.iv_mirror).setOnClickListener(view -> {
            mRoomEngineService.setCameraMirror();
        });
    }
}

