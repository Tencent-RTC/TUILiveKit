package com.trtc.uikit.livekit.voiceroom.view.preview;

import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.component.audioeffect.AudioEffectPanel;
import com.trtc.uikit.component.music.MusicPanelView;
import com.trtc.uikit.livekit.voiceroom.api.Constants;
import com.trtc.uikit.livekit.voiceroom.view.BasicView;

import java.util.Arrays;

public class AnchorPreviewFunctionView extends BasicView {
    private SettingsDialog          mSettingsDialog;
    private PopupDialog             mAudioEffectPanel;
    private PopupDialog             mMusicPanel;
    private StreamPresetImagePicker mStreamPresetImagePicker;

    public AnchorPreviewFunctionView(@NonNull Context context) {
        this(context, null);
    }

    public AnchorPreviewFunctionView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AnchorPreviewFunctionView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
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
                config.currentImageUrl = transferThumbUrlFromImage(mRoomState.backgroundURL.get());
                mStreamPresetImagePicker = new StreamPresetImagePicker(mContext, config);
                mStreamPresetImagePicker.setOnConfirmListener(imageUrl
                        -> mVoiceRoomManager.getRoomManager().setBackgroundURL(
                        transferImageUrlFromThumb(imageUrl)));
            }
            mStreamPresetImagePicker.show();
        });
    }

    private void initAudioEffectButton() {
        findViewById(R.id.iv_audio_effect).setOnClickListener(view -> {
            if (mAudioEffectPanel == null) {
                mAudioEffectPanel = new PopupDialog(mContext);
                AudioEffectPanel audioEffectPanel = new AudioEffectPanel(mContext);
                audioEffectPanel.init(mRoomState.roomId);
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
                MusicPanelView musicListPanelView = new MusicPanelView(mContext);
                musicListPanelView.init(mRoomState.roomId);
                mMusicPanel.setView(musicListPanelView);
            }
            mMusicPanel.show();
        });
    }

    private void initSettingsButton() {
        findViewById(R.id.iv_settings).setOnClickListener(view -> {
            if (mSettingsDialog == null) {
                mSettingsDialog = new SettingsDialog(mContext, mVoiceRoomManager);
            }
            mSettingsDialog.show();
        });
    }

    private String transferThumbUrlFromImage(String imageUrl) {
        if (TextUtils.isEmpty(imageUrl)) {
            return imageUrl;
        }

        int index = imageUrl.indexOf(".png");
        if (index == -1) {
            return imageUrl;
        }
        return imageUrl.substring(0, index) + "_thumb.png";
    }

    private String transferImageUrlFromThumb(String thumbUrl) {
        if (TextUtils.isEmpty(thumbUrl)) {
            return thumbUrl;
        }

        int index = thumbUrl.indexOf("_thumb.png");
        if (index == -1) {
            return thumbUrl;
        }
        return thumbUrl.substring(0, index) + ".png";
    }
}

