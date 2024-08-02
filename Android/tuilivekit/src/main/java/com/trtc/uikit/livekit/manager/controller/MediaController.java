package com.trtc.uikit.livekit.manager.controller;

import static com.tencent.liteav.beauty.TXBeautyManager.TXBeautyStyleSmooth;

import android.content.Context;
import android.view.View;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUIConfig;
import com.tencent.qcloud.tuicore.permission.PermissionCallback;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.common.uicomponent.beauty.TEBeautyService;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.common.utils.PermissionRequest;
import com.trtc.uikit.livekit.manager.error.ErrorHandler;
import com.trtc.uikit.livekit.service.ILiveService;
import com.trtc.uikit.livekit.state.LiveDefine;
import com.trtc.uikit.livekit.state.LiveState;

public class MediaController extends Controller {
    private static final String TAG = "MediaController";

    private final Observer<LiveDefine.LinkStatus> mLinkStateObserver = this::onLinkStateChanged;
    private final TEBeautyService                 mTEBeautyService;

    public MediaController(LiveState state, ILiveService service) {
        super(state, service);
        mViewState.linkStatus.observe(mLinkStateObserver);
        mTEBeautyService = new TEBeautyService(state, service.getTRTCCloud());
    }

    @Override
    public void destroy() {
        LiveKitLog.info(TAG + " destroy");
        mViewState.linkStatus.removeObserver(mLinkStateObserver);
        mTEBeautyService.clearBeautyView();
        closeLocalCamera();
    }

    public void operateMicrophone() {
        boolean isMicrophoneOpened = mMediaState.isMicrophoneOpened.get();
        if (!isMicrophoneOpened) {
            openLocalMicrophone();
            return;
        }
        boolean isMuted = mMediaState.isMicrophoneMuted.get();
        if (isMuted) {
            unMuteLocalAudio();
        } else {
            muteLocalAudio();
        }
    }

    public void setLocalVideoView(TUIVideoView view) {
        mLiveService.setLocalVideoView(view);
    }

    public void setRemoteVideoView(String userId, TUIRoomDefine.VideoStreamType streamType, TUIVideoView videoView) {
        mLiveService.setRemoteVideoView(userId, streamType, videoView);
    }

    public void startPlayRemoteVideo(String userId, TUIRoomDefine.VideoStreamType streamType,
                                     TUIRoomDefine.PlayCallback callback) {
        mLiveService.startPlayRemoteVideo(userId, streamType, callback);
    }

    public void openLocalCamera() {
        boolean hasCameraPermission = mMediaState.hasCameraPermission.get();
        if (hasCameraPermission) {
            openLocalCameraByService();
            return;
        }
        LiveKitLog.info(TAG + " requestPermissions:[]");
        PermissionRequest.requestPermissions(TUIConfig.getAppContext(), new PermissionCallback() {
            @Override
            public void onRequesting() {
                LiveKitLog.info(TAG + " requestPermissions:[onRequesting]");
            }

            @Override
            public void onGranted() {
                LiveKitLog.info(TAG + " requestPermissions:[onGranted]");
                mMediaState.hasCameraPermission.set(true);
                openLocalCameraByService();
            }

            @Override
            public void onDenied() {
                LiveKitLog.error(TAG + " requestPermissions:[onDenied]");
            }
        });
    }

    public void closeLocalCamera() {
        mLiveService.closeLocalCamera();
        mMediaState.isCameraOpened.set(false);
    }

    public void switchCamera() {
        boolean isFrontCamera = mMediaState.isFrontCamera.get();
        mLiveService.switchCamera(!isFrontCamera);
        mMediaState.isFrontCamera.set(!isFrontCamera);
    }

    public void setCameraMirror() {
        boolean isMirror = mMediaState.isMirror.get();
        mLiveService.setCameraMirror(!isMirror);
        mMediaState.isMirror.set(!isMirror);
    }

    public void updateVideoQuality(TUIRoomDefine.VideoQuality quality) {
        mLiveService.updateVideoQuality(quality);
        mMediaState.videoQuality.set(quality);
    }

    public void updateAudioQuality(TUIRoomDefine.AudioQuality quality) {
        mLiveService.updateAudioQuality(quality);
        mMediaState.audioQuality.set(quality);
    }

    public void setBeautyLevel(int level) {
        mLiveService.setBeautyLevel(level);
        mBeautyState.smoothLevel.set(level);
    }

    public void setWhitenessLevel(int level) {
        mLiveService.setWhitenessLevel(level);
        mBeautyState.whitenessLevel.set(level);
    }

    public void setRuddyLevel(int level) {
        mLiveService.setRuddyLevel(level);
        mBeautyState.ruddyLevel.set(level);
    }

    public void closeBeauty() {
        mBeautyState.smoothLevel.set(0);
        mLiveService.setBeautyLevel(0);
        mBeautyState.whitenessLevel.set(0);
        mLiveService.setWhitenessLevel(0);
        mBeautyState.ruddyLevel.set(0);
        mLiveService.setRuddyLevel(0);
    }

    public void setCustomVideoProcess() {
        mTEBeautyService.setCustomVideoProcess();
    }

    public View getTEBeautyView(Context context) {
        return mTEBeautyService.getBeautyView(context);
    }

    private void unMuteLocalAudio() {
        mLiveService.unMuteLocalAudio(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mMediaState.isMicrophoneMuted.set(false);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
            }
        });
    }

    private void muteLocalAudio() {
        mLiveService.muteLocalAudio();
        mMediaState.isMicrophoneMuted.set(true);
    }

    private void openLocalMicrophone() {
        boolean hasMicrophonePermission = mMediaState.hasMicrophonePermission.get();
        if (hasMicrophonePermission) {
            openLocalMicrophoneByService();
            return;
        }
        LiveKitLog.info(TAG + " requestMicrophonePermissions:[]");
        PermissionRequest.requestMicrophonePermissions(TUIConfig.getAppContext(), new PermissionCallback() {
            @Override
            public void onRequesting() {
                LiveKitLog.info(TAG + " requestMicrophonePermissions:[onRequesting}");
            }

            @Override
            public void onGranted() {
                LiveKitLog.info(TAG + " requestMicrophonePermissions:[onGranted]");
                mMediaState.hasMicrophonePermission.set(true);
                openLocalMicrophoneByService();
            }

            @Override
            public void onDenied() {
                LiveKitLog.warn(TAG + " requestMicrophonePermissions:[onDenied]");
            }
        });
    }

    private void openLocalMicrophoneByService() {
        unMuteLocalAudio();
        mLiveService.openLocalMicrophone(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mMediaState.isMicrophoneOpened.set(true);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
            }
        });
    }

    private void openLocalCameraByService() {
        if (mMediaState.isCameraOpened.get()) {
            LiveKitLog.error(TAG + " camera is opened, no need to open again");
            return;
        }
        mTEBeautyService.initBeautyKit();
        boolean isFront = mMediaState.isFrontCamera.get();
        TUIRoomDefine.VideoQuality quality = mMediaState.videoQuality.get();
        mLiveService.openLocalCamera(isFront, quality, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                initLivingConfig();
                mMediaState.isCameraOpened.set(true);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
            }
        });
    }

    private void initLivingConfig() {
        mLiveService.enableGravitySensor(true);
        mLiveService.setVideoResolutionMode(TUIRoomDefine.ResolutionMode.PORTRAIT);
        mLiveService.setBeautyStyle(TXBeautyStyleSmooth);
        updateVideoQuality(mMediaState.videoQuality.get());
        setBeautyLevel(mBeautyState.smoothLevel.get());
        setWhitenessLevel(mBeautyState.whitenessLevel.get());
        setRuddyLevel(mBeautyState.ruddyLevel.get());
        updateAudioQuality(mMediaState.audioQuality.get());
    }

    private void onLinkStateChanged(LiveDefine.LinkStatus status) {
        LiveKitLog.info(TAG + " onUserStateChanged status:" + status);
        if (status == LiveDefine.LinkStatus.LINKING) {

            if (mViewState.autoOpenCameraOnSeated.get()) {
                LiveKitLog.info(TAG + " requestPermissions:[Camera, Microphone]}");
                PermissionRequest.requestPermissions(TUIConfig.getAppContext(), new PermissionCallback() {
                    @Override
                    public void onRequesting() {
                        LiveKitLog.info(TAG + " requestPermissions:[Camera, Microphone]:[onRequesting}");
                    }

                    @Override
                    public void onGranted() {
                        LiveKitLog.info(TAG + " requestPermissions:[Camera, Microphone]:[onGranted]");
                        mMediaState.hasMicrophonePermission.set(true);
                        mMediaState.hasCameraPermission.set(true);
                        openLocalMicrophone();
                        openLocalCamera();
                    }

                    @Override
                    public void onDenied() {
                        LiveKitLog.warn(TAG + " requestPermissions:[Camera, Microphone]:[onDenied]");
                    }

                });
            } else {
                openLocalMicrophone();
            }
        }
    }
}
