package com.trtc.uikit.livekit.livestream.view.anchor.preview;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.component.audioeffect.AudioEffectPanel;
import com.trtc.uikit.livekit.component.beauty.BeautyViewFactory;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.view.widgets.videosettings.VideoSettingsDialog;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

public class PreviewFunctionView extends FrameLayout {
    private PopupDialog       mAudioEffectPanel;
    private PopupDialog       mPopupDialog;
    private View              mBeautyView;
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
            if (mPopupDialog == null) {
                mPopupDialog = new PopupDialog(getContext(), com.trtc.tuikit.common.R.style.TUICommonBottomDialogTheme);
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
                mBeautyView = beautyViewFactory.getBeautyView(getContext(), mLiveManager);
            }
            mPopupDialog.setView(mBeautyView);
            mPopupDialog.show();
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
            mLiveManager.getMediaManager().switchCamera();
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

