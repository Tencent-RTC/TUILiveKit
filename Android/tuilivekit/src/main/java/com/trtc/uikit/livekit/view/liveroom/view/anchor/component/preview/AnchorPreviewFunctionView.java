package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.preview;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.audioeffect.view.AudioEffectPanelView;
import com.trtc.uikit.livekit.common.uicomponent.beauty.BeautyViewFactory;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;

@SuppressLint("ViewConstructor")
public class AnchorPreviewFunctionView extends BasicView {

    private PopupDialog mAudioEffectPanel;
    private PopupDialog mPopupDialog;
    private View        mBeautyView;

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
            if (mPopupDialog == null) {
                mPopupDialog = new PopupDialog(mContext, com.trtc.tuikit.common.R.style.TUICommonBottomDialogTheme);
                mPopupDialog.setOnDismissListener(dialog -> {
                    if (mBeautyView != null) {
                        ViewGroup parentView = (ViewGroup) mBeautyView.getParent();
                        if (parentView != null) {
                            parentView.removeView(mBeautyView);
                        }
                    }
                    mPopupDialog = null;
                });
                BeautyViewFactory beautyViewFactory = new BeautyViewFactory();
                mBeautyView = beautyViewFactory.getBeautyView(mContext, mLiveController);
            }
            mPopupDialog.setView(mBeautyView);
            mPopupDialog.show();
        });
    }

    private void initAudioEffectButton() {
        findViewById(R.id.iv_audio_effect).setOnClickListener(view -> {
            if (mAudioEffectPanel == null) {
                mAudioEffectPanel = new PopupDialog(mContext);
                AudioEffectPanelView audioEffectPanel = new AudioEffectPanelView(mContext,
                        mLiveController.getRoomState().roomId, mLiveController.getLiveService().getTRTCCloud());
                audioEffectPanel.setOnBackButtonClickListener(() -> mAudioEffectPanel.dismiss());
                mAudioEffectPanel.setView(audioEffectPanel);
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

