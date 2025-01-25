package com.trtc.uikit.livekit.livestream.view.audience.playing.coguest.dialog;

import static com.trtc.uikit.livekit.livestream.state.CoGuestState.CoGuestStatus.APPLYING;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.constraintlayout.widget.ConstraintLayout;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.manager.error.ErrorHandler;
import com.trtc.uikit.livekit.livestream.view.audience.playing.coguest.settings.VideoCoGuestSettingsDialog;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

@SuppressLint("ViewConstructor")
public class TypeSelectDialog extends PopupDialog {
    private       ImageView         mImageLinkSettings;
    private       ConstraintLayout  mLayoutLinkVideo;
    private       ConstraintLayout  mLayoutLinkAudio;
    private final LiveCoreView      mLiveStream;
    private final LiveStreamManager mLiveManager;

    public TypeSelectDialog(@NonNull Context context, LiveStreamManager manager, LiveCoreView liveStream) {
        super(context);
        mLiveManager = manager;
        mLiveStream = liveStream;
        initView();
    }

    protected void initView() {
        @SuppressLint("InflateParams")
        View view = LayoutInflater.from(getContext()).inflate(R.layout.livekit_dialog_link_mic_selector, null);
        bindViewId(view);

        initLinkSettingsView();
        initLinkVideoView();
        initLinkAudioView();

        setView(view);
    }

    private void bindViewId(View view) {
        mImageLinkSettings = view.findViewById(R.id.iv_link_settings);
        mLayoutLinkVideo = view.findViewById(R.id.cl_link_video);
        mLayoutLinkAudio = view.findViewById(R.id.cl_link_audio);
    }

    private void initLinkAudioView() {
        mLayoutLinkAudio.setOnClickListener(view -> {
            if (!view.isEnabled()) {
                return;
            }
            view.setEnabled(false);
            applyLinkMic(false);
        });
    }

    private void initLinkVideoView() {
        mLayoutLinkVideo.setOnClickListener(view -> {
            if (!view.isEnabled()) {
                return;
            }
            view.setEnabled(false);
            applyLinkMic(true);
        });
    }

    private void initLinkSettingsView() {
        mImageLinkSettings.setOnClickListener(view -> {
            VideoCoGuestSettingsDialog settingsDialog = new VideoCoGuestSettingsDialog(getContext(), mLiveManager,
                    mLiveStream);
            settingsDialog.show();
            dismiss();
        });
    }

    private void applyLinkMic(boolean openCamera) {
        ToastUtil.toastShortMessageCenter(getContext().getString(R.string.livekit_toast_apply_link_mic));
        mLiveStream.requestIntraRoomConnection("", 60, openCamera, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mLiveManager.getCoGuestManager().updateCoGuestStates(APPLYING);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
            }
        });
        dismiss();
    }
}