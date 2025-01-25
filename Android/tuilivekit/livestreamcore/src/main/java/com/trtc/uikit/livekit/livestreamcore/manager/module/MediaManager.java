package com.trtc.uikit.livekit.livestreamcore.manager.module;

import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.VideoQuality.Q_1080P;
import static com.tencent.liteav.beauty.TXBeautyManager.TXBeautyStyleSmooth;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUIConfig;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.permission.PermissionCallback;
import com.trtc.uikit.livekit.livestreamcore.common.utils.Logger;
import com.trtc.uikit.livekit.livestreamcore.common.utils.PermissionRequest;
import com.trtc.uikit.livekit.livestreamcore.manager.api.ILiveStream;
import com.trtc.uikit.livekit.livestreamcore.state.LiveStreamState;
import com.trtc.uikit.livekit.livestreamcore.state.MediaState;

import java.util.HashMap;
import java.util.Map;

public class MediaManager extends BaseManager {
    private static final String     TAG = "MediaManager";
    public final         MediaState mMediaState;

    public MediaManager(LiveStreamState state, ILiveStream service) {
        super(state, service);
        mMediaState = state.mediaState;
        initVideoAdvanceSettings();
    }

    @Override
    public void destroy() {
        Logger.info(TAG + " destroy");
        closeLocalCamera();
        unInitVideoAdvanceSettings();
    }

    public void updateVideoQualityEx(TUIRoomDefine.RoomVideoEncoderParams videoEncParams) {
        mMediaState.videoEncParams.setCurrentEnc(videoEncParams);
        mVideoLiveService.updateVideoQualityEx(videoEncParams);
    }

    public void enableMirror(boolean enable) {
        mVideoLiveService.setCameraMirror(enable);
        mMediaState.isMirror.set(enable);
    }

    public void enableAdvancedVisible(boolean visible) {
        mMediaState.videoAdvanceSetting.isVisible = visible;
    }

    public void enableUltimate(boolean enable) {
        mMediaState.videoAdvanceSetting.isUltimateEnabled = enable;
        Map<String, Object> params = new HashMap<>();
        params.put("enable", enable);
        TUICore.callService("VideoAdvanceExtension", "enableUltimate", params);

        if(enable){
            enableBFrame(false);
        }
    }

    public void enableBFrame(boolean enable) {
        mMediaState.videoAdvanceSetting.isBFrameEnabled = enable;
        Map<String, Object> params = new HashMap<>();
        params.put("enable", enable);
        TUICore.callService("VideoAdvanceExtension", "enableBFrame", params);
    }

    public void enableH265(boolean enable) {
        mMediaState.videoAdvanceSetting.isH265Enabled = enable;
        Map<String, Object> params = new HashMap<>();
        params.put("enable", enable);
        TUICore.callService("VideoAdvanceExtension", "enableH265", params);
    }

    public void openLocalCamera(boolean useFrontCamera, TUIRoomDefine.ActionCallback callback) {
        if (mVideoLiveState.mediaState.isFrontCamera.get() != useFrontCamera
                && mVideoLiveState.mediaState.isCameraOpened.get()) {
            switchCamera();
            if (callback != null) {
                callback.onSuccess();
            }
            return;
        }
        mVideoLiveState.mediaState.isFrontCamera.set(useFrontCamera);
        boolean hasCameraPermission = mVideoLiveState.mediaState.hasCameraPermission.get();
        if (hasCameraPermission) {
            openLocalCameraByService(callback);
            return;
        }
        Logger.info(TAG + " requestPermissions:[]");
        PermissionRequest.requestPermissions(TUIConfig.getAppContext(), new PermissionCallback() {
            @Override
            public void onRequesting() {
                Logger.info(TAG + " requestPermissions:[onRequesting]");
            }

            @Override
            public void onGranted() {
                Logger.info(TAG + " requestPermissions:[onGranted]");
                mVideoLiveState.mediaState.hasCameraPermission.set(true);
                openLocalCameraByService(callback);
            }

            @Override
            public void onDenied() {
                Logger.error(TAG + " requestPermissions:[onDenied]");
                if (callback != null) {
                    callback.onError(TUICommonDefine.Error.PERMISSION_DENIED, "requestPermissions:[onDenied]");
                }
            }
        });
    }

    public void closeLocalCamera() {
        mVideoLiveService.closeLocalCamera();
        mVideoLiveState.mediaState.isCameraOpened.set(false);
    }

    public void setLocalVideoView(TUIVideoView view) {
        addLocalVideoListener(view);
        mVideoLiveService.setLocalVideoView(view);
    }

    public void setRemoteVideoView(String userId, TUIRoomDefine.VideoStreamType streamType, TUIVideoView videoView) {
        mVideoLiveService.setRemoteVideoView(userId, streamType, videoView);
    }

    public void startPlayRemoteVideo(String userId, TUIRoomDefine.VideoStreamType streamType,
                                     TUIRoomDefine.PlayCallback callback) {
        mVideoLiveService.startPlayRemoteVideo(userId, streamType, callback);
    }

    public void stopPlayRemoteVideo(String userId, TUIRoomDefine.VideoStreamType streamType) {
        mVideoLiveService.stopPlayRemoteVideo(userId, streamType);
    }

    public void openLocalMicrophone(TUIRoomDefine.ActionCallback callback) {
        boolean hasMicrophonePermission = mVideoLiveState.mediaState.hasMicrophonePermission.get();
        if (hasMicrophonePermission) {
            openLocalMicrophoneByService(callback);
            return;
        }
        Logger.info(TAG + " requestMicrophonePermissions:[]");
        PermissionRequest.requestMicrophonePermissions(TUIConfig.getAppContext(), new PermissionCallback() {
            @Override
            public void onRequesting() {
                Logger.info(TAG + " requestMicrophonePermissions:[onRequesting}");
            }

            @Override
            public void onGranted() {
                Logger.info(TAG + " requestMicrophonePermissions:[onGranted]");
                mVideoLiveState.mediaState.hasMicrophonePermission.set(true);
                openLocalMicrophoneByService(callback);
            }

            @Override
            public void onDenied() {
                Logger.warn(TAG + " requestMicrophonePermissions:[onDenied]");
                if (callback != null) {
                    callback.onError(TUICommonDefine.Error.PERMISSION_DENIED, "requestMicrophonePermissions:[onDenied"
                            + "]");
                }
            }
        });
    }

    public void closeLocalMicrophone() {
        mVideoLiveService.closeLocalMicrophone();
        mVideoLiveState.mediaState.isMicrophoneOpened.set(false);
    }

    public void unMuteLocalAudio() {
        mVideoLiveService.unMuteLocalAudio(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mVideoLiveState.mediaState.isMicrophoneMuted.set(false);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
            }
        });
    }

    public void muteLocalAudio(boolean mute) {
        if (mute) {
            mVideoLiveService.muteLocalAudio();
            mVideoLiveState.mediaState.isMicrophoneMuted.set(true);
        } else {
            unMuteLocalAudio();
        }
    }

    public void switchCamera() {
        boolean isFrontCamera = mMediaState.isFrontCamera.get();
        mVideoLiveService.switchCamera(!isFrontCamera);
        mMediaState.isFrontCamera.set(!isFrontCamera);
    }

    private void initVideoAdvanceSettings() {
        enableUltimate(true);
        enableH265(true);
    }

    private void unInitVideoAdvanceSettings() {
        enableUltimate(false);
        enableH265(false);

        TUIRoomDefine.RoomVideoEncoderParams roomVideoEncoderParams = new TUIRoomDefine.RoomVideoEncoderParams();
        roomVideoEncoderParams.videoResolution = Q_1080P;
        roomVideoEncoderParams.bitrate = 6000;
        roomVideoEncoderParams.fps = 30;
        roomVideoEncoderParams.resolutionMode = TUIRoomDefine.ResolutionMode.PORTRAIT;
        mVideoLiveService.updateVideoQualityEx(roomVideoEncoderParams);
    }

    private void addLocalVideoListener(TUIVideoView view) {
        if (view != null) {
            final int screenWidth = view.getContext().getResources().getDisplayMetrics().widthPixels;

            view.getViewTreeObserver().addOnGlobalLayoutListener(() -> {
                MediaState.VideoEncParams.VideoEncType targetEncType = (double) view.getWidth() / screenWidth > 0.5 ?
                        MediaState.VideoEncParams.VideoEncType.BIG : MediaState.VideoEncParams.VideoEncType.SMALL;
                if (mMediaState.videoEncParams.currentEncType != targetEncType) {
                    changeVideoEncParams(targetEncType);
                }
            });
        }
    }

    private void changeVideoEncParams(MediaState.VideoEncParams.VideoEncType encType) {
        mMediaState.videoEncParams.currentEncType = encType;
        if (encType == MediaState.VideoEncParams.VideoEncType.BIG) {
            updateVideoQualityEx(mMediaState.videoEncParams.big);
        } else if (encType == MediaState.VideoEncParams.VideoEncType.SMALL) {
            updateVideoQualityEx(mMediaState.videoEncParams.small);
        }
    }

    private void openLocalMicrophoneByService(TUIRoomDefine.ActionCallback callback) {
        unMuteLocalAudio();
        mVideoLiveService.openLocalMicrophone(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mVideoLiveState.mediaState.isMicrophoneOpened.set(true);
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    private void openLocalCameraByService(TUIRoomDefine.ActionCallback callback) {
        if (mVideoLiveState.mediaState.isCameraOpened.get()) {
            Logger.error(TAG + " camera is opened, no need to open again");
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.PERMISSION_DENIED, "camera not permissions");
            }
            return;
        }
        boolean isFront = mVideoLiveState.mediaState.isFrontCamera.get();
        TUIRoomDefine.VideoQuality quality = Q_1080P;
        mVideoLiveService.openLocalCamera(isFront, quality, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                initLivingConfig();
                mVideoLiveState.mediaState.isCameraOpened.set(true);
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    private void initLivingConfig() {
        mVideoLiveService.enableGravitySensor(true);
        mVideoLiveService.setVideoResolutionMode(TUIRoomDefine.ResolutionMode.PORTRAIT);
        mVideoLiveService.setBeautyStyle(TXBeautyStyleSmooth);
        mVideoLiveService.updateVideoQuality(Q_1080P);
        mVideoLiveService.updateAudioQuality(TUIRoomDefine.AudioQuality.DEFAULT);
        mVideoLiveService.setCameraMirror(true);
    }
}
