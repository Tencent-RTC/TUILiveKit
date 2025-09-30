package com.trtc.uikit.livekit.features.audiencecontainer.view.coguest.panel;

import static com.trtc.uikit.livekit.features.audiencecontainer.state.CoGuestState.CoGuestStatus.APPLYING;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.Button;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.permission.PermissionCallback;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.common.PermissionRequest;
import com.trtc.uikit.livekit.common.ui.RoundFrameLayout;
import com.trtc.uikit.livekit.component.beauty.BeautyUtils;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.AudienceManager;

import io.trtc.tuikit.atomicxcore.api.LiveCoreView;

@SuppressLint("ViewConstructor")
public class VideoCoGuestSettingsDialog extends PopupDialog implements AudienceManager.AudienceViewListener {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("VideoCoGuestSettingsDialog");

    private       RoundFrameLayout mRoundFrameLayout;
    private       TUIVideoView     mPreviewVideoView;
    private       Button           mButtonApplyLinkMic;
    private       RecyclerView     mRecycleSettingsOption;
    private final Context          mContext;
    private final LiveCoreView     mLiveStream;
    private final AudienceManager  mAudienceManager;

    public VideoCoGuestSettingsDialog(@NonNull Context context, AudienceManager manager, LiveCoreView liveCoreView) {
        super(context);
        mContext = context;
        mAudienceManager = manager;
        mLiveStream = liveCoreView;
        initView();
    }

    protected void initView() {
        View view = LayoutInflater.from(getContext()).inflate(R.layout.livekit_dialog_link_video_settings, null);
        bindViewId(view);

        initRecycleSettingsOption();
        initPreviewVideoView();
        initApplyLinkMicButton();
        initRoundFrameLayout();

        setView(view);
    }

    private void bindViewId(View view) {
        mPreviewVideoView = view.findViewById(R.id.preview_audience_video);
        mButtonApplyLinkMic = view.findViewById(R.id.btn_apply_link_mic);
        mRecycleSettingsOption = view.findViewById(R.id.video_settings_options);
        mRoundFrameLayout = view.findViewById(R.id.fl_preview_audience_video);
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
        mLiveStream.stopCamera();
    }

    private void initRoundFrameLayout() {
        mRoundFrameLayout.setRadius(ScreenUtil.dip2px(16));
    }

    private void initApplyLinkMicButton() {
        mButtonApplyLinkMic.setOnClickListener(view -> {
            if (!view.isEnabled()) {
                return;
            }
            view.setEnabled(false);
            ToastUtil.toastShortMessageCenter(getContext().getString(R.string.common_toast_apply_link_mic));
            LOGGER.info("requestMicrophonePermissions success");
            PermissionRequest.requestCameraPermissions(ContextProvider.getApplicationContext(),
                    new PermissionCallback() {
                        @Override
                        public void onGranted() {
                            LOGGER.info("requestCameraPermissions:[onGranted]");
                            PermissionRequest.requestMicrophonePermissions(ContextProvider.getApplicationContext(),
                                    new PermissionCallback() {
                                        @Override
                                        public void onGranted() {
                                            mLiveStream.requestIntraRoomConnection("", 60, true,
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
                        }

                        @Override
                        public void onDenied() {
                            LOGGER.error("requestCameraPermissions:[onDenied]");
                        }
                    });
            dismiss();
        });
    }

    private void initPreviewVideoView() {
        mAudienceManager.getMediaManager().setLocalVideoView(mPreviewVideoView);
        boolean isFront = Boolean.TRUE.equals(mLiveStream.getCoreState().mediaState.isFrontCamera.getValue());
        PermissionRequest.requestCameraPermissions(ContextProvider.getApplicationContext(), new PermissionCallback() {
            @Override
            public void onGranted() {
                mLiveStream.startCamera(isFront, null);
            }
        });

    }

    private void initRecycleSettingsOption() {
        mRecycleSettingsOption.setLayoutManager(new GridLayoutManager(mContext, 2));
        VideoCoGuestSettingsAdapter adapter = new VideoCoGuestSettingsAdapter(mContext, mAudienceManager,
                mLiveStream);
        adapter.setOnItemClickListener(mOnItemClickListener);
        mRecycleSettingsOption.setAdapter(adapter);
    }

    @Override
    public void onRoomDismissed(String roomId) {
        dismiss();
        BeautyUtils.resetBeauty();
        BeautyUtils.dismissBeautyDialog();
    }

    private final VideoCoGuestSettingsAdapter.OnItemClickListener mOnItemClickListener =
            new VideoCoGuestSettingsAdapter.OnItemClickListener() {
                @Override
                public void onBeautyItemClicked() {
                    BeautyUtils.showBeautyDialog(mContext);
                }

                @Override
                public void onFlipItemClicked() {
                    boolean isFront =
                            Boolean.TRUE.equals(mLiveStream.getCoreState().mediaState.isFrontCamera.getValue());
                    mLiveStream.switchCamera(!isFront);
                }
            };

}
