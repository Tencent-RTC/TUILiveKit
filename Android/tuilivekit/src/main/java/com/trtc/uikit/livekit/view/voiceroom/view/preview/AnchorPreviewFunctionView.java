package com.trtc.uikit.livekit.view.voiceroom.view.preview;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.audioeffect.view.AudioEffectPanelView;
import com.trtc.uikit.livekit.common.uicomponent.music.view.MusicPanelView;
import com.trtc.uikit.livekit.common.uicomponent.preview.StreamPresetImagePicker;
import com.trtc.uikit.livekit.common.utils.BackgroundImageUtils;
import com.trtc.uikit.livekit.common.utils.Constants;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.common.view.BottomPanel;
import com.trtc.uikit.livekit.manager.LiveController;

import java.util.Arrays;

@SuppressLint("ViewConstructor")
public class AnchorPreviewFunctionView extends BasicView {

    private BottomPanel             mSettingsPanel;
    private PopupDialog             mAudioEffectPanel;
    private PopupDialog             mMusicPanel;
    private StreamPresetImagePicker mStreamPresetImagePicker;

    public AnchorPreviewFunctionView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(getContext()).inflate(R.layout.livekit_voiceroom_anchor_preview_function, this, true);
        initBackgroundImageButton();
        initBackgroundMusicButton();
        initAudioEffectButton();
        initSettingsButton();
    }

    @Override
    protected void addObserver() {

    }

    @Override
    protected void removeObserver() {

    }

    private void initBackgroundImageButton() {
        findViewById(R.id.iv_bg_image).setOnClickListener(view -> {
            if (mStreamPresetImagePicker == null) {
                StreamPresetImagePicker.Config config = new StreamPresetImagePicker.Config();
                config.title = mContext.getString(R.string.livekit_settings_bg_image);
                config.confirmButtonText = mContext.getString(R.string.livekit_set_as_background);
                config.data = Arrays.asList(Constants.BACKGROUND_THUMB_URL_LIST);
                config.currentImageUrl = BackgroundImageUtils.transferThumbUrlFromImage(mRoomState.backgroundURL.get());
                mStreamPresetImagePicker = new StreamPresetImagePicker(mContext, config);
                mStreamPresetImagePicker.setOnItemClickListener(imageUrl
                        -> mLiveController.getRoomController().setBackgroundURL(imageUrl));
            }
            mStreamPresetImagePicker.show();
        });
    }

    private void initAudioEffectButton() {
        findViewById(R.id.iv_audio_effect).setOnClickListener(view -> {
            if (mAudioEffectPanel == null) {
                mAudioEffectPanel = new PopupDialog(mContext);
                AudioEffectPanelView audioEffectPanel = new AudioEffectPanelView(mContext, mRoomState.roomId,
                        mLiveController.getLiveService().getTRTCCloud());
                audioEffectPanel.setOnBackButtonClickListener(() -> mAudioEffectPanel.dismiss());
                mAudioEffectPanel.setView(audioEffectPanel);
            }
            mAudioEffectPanel.show();
        });
    }

    private void initBackgroundMusicButton() {
        findViewById(R.id.iv_bg_music).setOnClickListener(view -> {
            if (mMusicPanel == null) {
                mMusicPanel = new PopupDialog(mContext);
                MusicPanelView musicListPanelView = new MusicPanelView(mContext, mRoomState.roomId,
                        mLiveController.getLiveService().getTRTCCloud());
                mMusicPanel.setView(musicListPanelView);
            }
            mMusicPanel.show();
        });
    }

    private void initSettingsButton() {
        findViewById(R.id.iv_settings).setOnClickListener(view -> {
            if (mSettingsPanel == null) {
                SettingsPanelView panelView = new SettingsPanelView(mContext, mLiveController);
                mSettingsPanel = BottomPanel.create(panelView);
            }
            mSettingsPanel.show();
        });
    }
}

