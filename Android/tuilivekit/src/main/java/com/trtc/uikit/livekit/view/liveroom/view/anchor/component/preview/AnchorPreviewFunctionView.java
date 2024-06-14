package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.preview;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.common.uicomponent.audioeffect.view.AudioEffectPanelView;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.common.view.BottomPanel;
import com.trtc.uikit.livekit.view.liveroom.view.anchor.component.common.BeautyListPanel;

@SuppressLint("ViewConstructor")
public class AnchorPreviewFunctionView extends BasicView {

    private BottomPanel     mAudioEffectPanel;
    private PopupDialog     mDialogBeautyList;
    private BeautyListPanel mBeautyListPanel;


    public AnchorPreviewFunctionView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(getContext()).inflate(R.layout.livekit_layout_anchor_preview_function, this,
                true);

        initBeautyButton();
        initAudioEffectButton();
        initFlipButton();
        initMirrorButton();
    }

    @Override
    protected void addObserver() {

    }

    @Override
    protected void removeObserver() {

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
                mBeautyListPanel = new BeautyListPanel(mContext, mLiveController);
            }
            mDialogBeautyList.setView(mBeautyListPanel);
            mDialogBeautyList.show();
        });
    }

    private void initAudioEffectButton() {
        findViewById(R.id.iv_audio_effect).setOnClickListener(view -> {
            if (mAudioEffectPanel == null) {
                AudioEffectPanelView audioEffectPanel = new AudioEffectPanelView(mContext, mLiveController);
                mAudioEffectPanel = BottomPanel.create(audioEffectPanel);
            }
            mAudioEffectPanel.show();
        });
    }

    private void initFlipButton() {
        findViewById(R.id.iv_flip).setOnClickListener(view -> {
            mLiveController.getMediaController().switchCamera();
        });
    }

    private void initMirrorButton() {
        findViewById(R.id.iv_mirror).setOnClickListener(view -> {
            mLiveController.getMediaController().setCameraMirror();
        });
    }
}

