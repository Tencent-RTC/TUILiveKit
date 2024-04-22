package com.trtc.uikit.livekit.liveroom.view.audience.component;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.LinearLayout;

import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.permission.PermissionCallback;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.utils.PermissionRequest;
import com.trtc.uikit.livekit.liveroom.core.LiveKitStore;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.core.TUILiveDefine.UserInteractionStatus;

@SuppressLint("ViewConstructor")
public class LinkMicTypePanel extends LinearLayout {
    private static final int SEAT_INDEX   = -1;
    private static final int REQ_TIME_OUT = 0;

    private final Context                          mContext;
    private final RoomEngineService                mRoomEngineService;
    private final LiveKitStore                     mStore;
    private       PopupDialog.DialogActionListener mListener;

    public LinkMicTypePanel(@NonNull Context context, RoomEngineService service) {
        super(context);
        mContext = context;
        mRoomEngineService = service;
        mStore = LiveKitStore.sharedInstance();
        LayoutInflater.from(context).inflate(R.layout.livekit_dialog_link_mic_selector, this, true);
        setOrientation(VERTICAL);
        setBackground(ContextCompat.getDrawable(context, R.drawable.livekit_dialog_background));
        findViewById(R.id.video_link_settings).setOnClickListener(view -> {
            PopupDialog videoLinkDialog = new PopupDialog(context);
            VideoLinkSettingsPanel videoLinkPanel = new VideoLinkSettingsPanel(context, service);
            videoLinkPanel.setDialogActionListener(videoLinkDialog::dismiss);
            videoLinkDialog.setView(videoLinkPanel);
            videoLinkDialog.show();
            mListener.dismiss();
        });

        findViewById(R.id.item_link_video).setOnClickListener(this::applyLinkMic);

        findViewById(R.id.item_link_audio).setOnClickListener(this::applyLinkMic);
    }

    private void applyLinkMic(View view) {
        mStore.setSelfStatus(UserInteractionStatus.APPLYING);
        ToastUtil.toastShortMessageCenter(mContext.getString(R.string.livekit_toast_apply_link_mic));
        TUIRoomDefine.Request request =
                mRoomEngineService.takeSeat(SEAT_INDEX, REQ_TIME_OUT,
                        new TUIRoomDefine.RequestCallback() {
                            @Override
                            public void onAccepted(String requestId, String userId) {
                                mStore.setSelfStatus(UserInteractionStatus.LINKING);
                                ToastUtil.toastShortMessageCenter(
                                        mContext.getString(R.string.livekit_toast_link_mic_success));
                                if (view.getId() == R.id.item_link_video) {
                                    openSelfCamera();
                                }
                                mRoomEngineService.openLocalMicrophone();
                            }

                            @Override
                            public void onRejected(String requestId, String userId, String message) {
                                mStore.setSelfStatus(UserInteractionStatus.NONE);
                                ToastUtil.toastShortMessageCenter(
                                        mContext.getString(R.string.livekit_link_apply_rejected));
                            }

                            @Override
                            public void onCancelled(String requestId, String userId) {
                                mStore.setSelfStatus(UserInteractionStatus.NONE);
                            }

                            @Override
                            public void onTimeout(String requestId, String userId) {
                                mStore.setSelfStatus(UserInteractionStatus.NONE);
                                ToastUtil.toastShortMessageCenter(
                                        mContext.getString(R.string.livekit_link_apply_timeout));
                            }

                            @Override
                            public void onError(String requestId, String userId,
                                                TUICommonDefine.Error error, String message) {
                                mStore.setSelfStatus(UserInteractionStatus.NONE);
                            }
                        });
        mStore.selfInfo.requestId = request.requestId;
        mListener.dismiss();
    }

    private void openSelfCamera() {
        PermissionRequest.requestPermissions(mContext.getApplicationContext(),
                new PermissionCallback() {
                    @Override
                    public void onGranted() {
                        mRoomEngineService.openLocalCamera(mStore.selfInfo.videoInfo.isFrontCamera.get(),
                                mStore.selfInfo.videoInfo.videoQuality.get(), null);
                    }
                });
    }

    public void setDialogActionListener(PopupDialog.DialogActionListener listener) {
        mListener = listener;
    }
}