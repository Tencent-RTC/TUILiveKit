package com.trtc.uikit.livekit.features.anchorprepare.view.function;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.audioeffect.AudioEffectPanel;
import com.trtc.uikit.livekit.component.beauty.BeautyUtils;
import com.trtc.uikit.livekit.component.beauty.tebeauty.TEBeautyManager;
import com.trtc.uikit.livekit.features.anchorprepare.manager.AnchorPrepareManager;
import com.trtc.uikit.livekit.features.anchorprepare.state.AnchorPrepareConfig;
import com.trtc.uikit.livekit.features.anchorprepare.view.liveinfoedit.livetemplatepicker.LiveTemplatePicker;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

public class PrepareFunctionView extends FrameLayout {
    private       PopupDialog          mAudioEffectPanel;
    private       AnchorPrepareManager mManager;
    private       LiveCoreView         mLiveCoreView;
    private final Observer<Boolean>    mDisableAudioEffectObserver = this::onAudioEffectDisableChange;
    private final Observer<Boolean>    mDisableBeautyObserver      = this::onBeautyDisableChange;
    private final Observer<Boolean>    mDisableMirrorObserver      = this::onMirrorDisableChange;

    public PrepareFunctionView(@NonNull Context context) {
        this(context, null);
    }

    public PrepareFunctionView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public PrepareFunctionView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        LayoutInflater.from(getContext()).inflate(R.layout.anchor_prepare_layout_function, this, true);
    }

    public void init(AnchorPrepareManager manager, LiveCoreView liveCoreView) {
        mManager = manager;
        mLiveCoreView = liveCoreView;
        TEBeautyManager.getInstance().setCustomVideoProcess();

        initView();
        addObserver();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void addObserver() {
        AnchorPrepareConfig.disableMenuAudioEffectButton.observeForever(mDisableAudioEffectObserver);
        AnchorPrepareConfig.disableMenuBeautyButton.observeForever(mDisableBeautyObserver);
        AnchorPrepareConfig.disableMenuSwitchButton.observeForever(mDisableMirrorObserver);
    }

    private void removeObserver() {
        AnchorPrepareConfig.disableMenuAudioEffectButton.removeObserver(mDisableAudioEffectObserver);
        AnchorPrepareConfig.disableMenuBeautyButton.removeObserver(mDisableBeautyObserver);
        AnchorPrepareConfig.disableMenuSwitchButton.removeObserver(mDisableMirrorObserver);
    }

    protected void initView() {
        initBeautyButton();
        initAudioEffectButton();
        initFlipButton();
        initLayoutButton();
    }

    private void initBeautyButton() {
        findViewById(R.id.iv_beauty).setOnClickListener(view -> {
            BeautyUtils.showBeautyDialog(getContext());
        });
    }

    private void initAudioEffectButton() {
        findViewById(R.id.iv_audio_effect).setOnClickListener(view -> {
            if (mAudioEffectPanel == null) {
                mAudioEffectPanel = new PopupDialog(getContext());
                AudioEffectPanel audioEffectPanel = new AudioEffectPanel(getContext());
                audioEffectPanel.init(mManager.getState().roomId);
                audioEffectPanel.setOnBackButtonClickListener(() -> mAudioEffectPanel.dismiss());
                mAudioEffectPanel.setView(audioEffectPanel);
            }
            mAudioEffectPanel.show();
        });
    }

    private void initFlipButton() {
        findViewById(R.id.iv_flip).setOnClickListener(view -> {
            boolean isFront = Boolean.TRUE.equals(mLiveCoreView.getCoreState().mediaState.isFrontCamera.getValue());
            mLiveCoreView.switchCamera(!isFront);
        });
    }

    private void initLayoutButton() {
        findViewById(R.id.iv_layout).setOnClickListener(view -> {
            LiveTemplatePicker picker = new LiveTemplatePicker(getContext(), mManager);
            picker.show();
        });
    }

    private void onMirrorDisableChange(Boolean disable) {
        if (Boolean.TRUE.equals(AnchorPrepareConfig.disableMenuSwitchButton.getValue())) {
            findViewById(R.id.rl_flip).setVisibility(GONE);
        } else {
            findViewById(R.id.rl_flip).setVisibility(VISIBLE);
        }
    }

    private void onAudioEffectDisableChange(Boolean disable) {
        if (Boolean.TRUE.equals(AnchorPrepareConfig.disableMenuAudioEffectButton.getValue())) {
            findViewById(R.id.rl_audio_effect).setVisibility(GONE);
        } else {
            findViewById(R.id.rl_audio_effect).setVisibility(VISIBLE);
        }
    }

    private void onBeautyDisableChange(Boolean disable) {
        if (Boolean.TRUE.equals(AnchorPrepareConfig.disableMenuBeautyButton.getValue())) {
            findViewById(R.id.rl_beauty).setVisibility(GONE);
        } else {
            findViewById(R.id.rl_beauty).setVisibility(VISIBLE);
        }
    }
}

