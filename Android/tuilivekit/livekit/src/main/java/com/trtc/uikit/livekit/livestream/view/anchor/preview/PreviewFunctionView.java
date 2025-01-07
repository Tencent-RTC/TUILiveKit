package com.trtc.uikit.livekit.livestream.view.anchor.preview;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.component.audioeffect.AudioEffectPanel;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.beauty.BeautyViewFactory;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.view.widgets.videosettings.VideoSettingsDialog;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

public class PreviewFunctionView extends FrameLayout {
    private PopupDialog       mAudioEffectPanel;
    private LiveStreamManager mLiveManager;
    private LiveCoreView      mLiveCoreView;

    public PreviewFunctionView(@NonNull Context context) {
        this(context, null);
    }

    public PreviewFunctionView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public PreviewFunctionView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        LayoutInflater.from(getContext()).inflate(R.layout.livekit_layout_anchor_preview_function, this, true);
    }

    public void init(LiveStreamManager manager, LiveCoreView liveCoreView) {
        mLiveManager = manager;
        mLiveCoreView = liveCoreView;
        initView();
    }

    protected void initView() {
        initBeautyButton();
        initAudioEffectButton();
        initFlipButton();
        initMirrorButton();
    }

    private void initBeautyButton() {
        findViewById(R.id.iv_beauty).setOnClickListener(view -> {
            BeautyViewFactory beautyViewFactory = new BeautyViewFactory();
            beautyViewFactory.showBeautyPanel(getContext(), mLiveManager);
        });
    }

    private void initAudioEffectButton() {
        findViewById(R.id.iv_audio_effect).setOnClickListener(view -> {
            if (mAudioEffectPanel == null) {
                mAudioEffectPanel = new PopupDialog(getContext());
                AudioEffectPanel audioEffectPanel = new AudioEffectPanel(getContext());
                audioEffectPanel.init(mLiveManager.getRoomState().roomId);
                audioEffectPanel.setOnBackButtonClickListener(() -> mAudioEffectPanel.dismiss());
                mAudioEffectPanel.setView(audioEffectPanel);
            }
            mAudioEffectPanel.show();
        });
    }

    private void initFlipButton() {
        findViewById(R.id.iv_flip).setOnClickListener(view -> {
            mLiveCoreView.getMediaManager().switchCamera();
            mLiveManager.getMediaManager().setFrontCamera(
                    mLiveCoreView.getMediaManager().mMediaState.isFrontCamera.get());
        });
    }

    private void initMirrorButton() {
        findViewById(R.id.iv_mirror).setOnClickListener(view -> {
            VideoSettingsDialog videoSettingsDialog =
                    new VideoSettingsDialog(getContext(), mLiveCoreView);
            videoSettingsDialog.show();
        });
    }
}

