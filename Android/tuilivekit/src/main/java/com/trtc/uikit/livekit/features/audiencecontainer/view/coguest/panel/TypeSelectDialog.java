package com.trtc.uikit.livekit.features.audiencecontainer.view.coguest.panel;

import static com.trtc.uikit.livekit.features.audiencecontainer.state.CoGuestState.CoGuestStatus.APPLYING;

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
import com.trtc.tuikit.common.permission.PermissionCallback;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.common.PermissionRequest;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.AudienceManager;

import io.trtc.tuikit.atomicxcore.api.LiveCoreView;

@SuppressLint("ViewConstructor")
public class TypeSelectDialog extends PopupDialog implements AudienceManager.AudienceViewListener {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("TypeSelectDialog");

    private       ImageView        mImageLinkSettings;
    private       ConstraintLayout mLayoutLinkVideo;
    private       ConstraintLayout mLayoutLinkAudio;
    private final LiveCoreView     mLiveStream;
    private final AudienceManager  mAudienceManager;
    private final Context          mContext;
    private final int              mSeatIndex;

    public TypeSelectDialog(@NonNull Context context, AudienceManager manager, LiveCoreView liveStream, int index) {
        super(context);
        mContext = context;
        mAudienceManager = manager;
        mLiveStream = liveStream;
        mSeatIndex = index;
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

    @Override
    public void onAttachedToWindow() {
        super.onAttachedToWindow();
        mAudienceManager.addAudienceViewListener(this);
    }

    @Override
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        mAudienceManager.removeAudienceViewListener(this);
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
            VideoCoGuestSettingsDialog settingsDialog = new VideoCoGuestSettingsDialog(mContext, mAudienceManager,
                    mLiveStream);
            settingsDialog.show();
            dismiss();
        });
    }

    private void applyLinkMic(boolean openCamera) {
        ToastUtil.toastShortMessageCenter(getContext().getString(R.string.common_toast_apply_link_mic));
        PermissionRequest.requestMicrophonePermissions(ContextProvider.getApplicationContext(),
                new PermissionCallback() {
                    @Override
                    public void onGranted() {
                        if (openCamera) {
                            PermissionRequest.requestCameraPermissions(ContextProvider.getApplicationContext(),
                                    new PermissionCallback() {
                                        @Override
                                        public void onGranted() {
                                            LOGGER.info("requestCameraPermissions:[onGranted]");
                                            mLiveStream.requestIntraRoomConnection("", mSeatIndex, 60, openCamera,
                                                    new TUIRoomDefine.ActionCallback() {
                                                        @Override
                                                        public void onSuccess() {
                                                            mAudienceManager.getCoGuestManager().updateCoGuestStates(APPLYING);
                                                        }

                                                        @Override
                                                        public void onError(TUICommonDefine.Error error,
                                                                            String message) {
                                                            ErrorLocalized.onError(error);
                                                        }
                                                    });
                                        }

                                        @Override
                                        public void onDenied() {
                                            LOGGER.error("requestCameraPermissions:[onDenied]");
                                        }
                                    });
                        } else {
                            mLiveStream.requestIntraRoomConnection("", mSeatIndex, 60, openCamera,
                                    new TUIRoomDefine.ActionCallback() {
                                        @Override
                                        public void onSuccess() {
                                            mAudienceManager.getCoGuestManager().updateCoGuestStates(APPLYING);
                                        }

                                        @Override
                                        public void onError(TUICommonDefine.Error error, String message) {
                                            ErrorLocalized.onError(error);
                                        }
                                    });
                        }
                    }

                    @Override
                    public void onDenied() {
                        LOGGER.error("requestCameraPermissions:[onDenied]");
                    }
                });
        dismiss();
    }

    @Override
    public void onRoomDismissed(String roomId) {
        dismiss();
    }
}