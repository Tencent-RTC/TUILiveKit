package com.trtc.uikit.livekit.features.anchorprepare.manager;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.tuikit.common.permission.PermissionCallback;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.common.PermissionRequest;
import com.trtc.uikit.livekit.features.anchorprepare.AnchorPrepareViewDefine;
import com.trtc.uikit.livekit.features.anchorprepare.AnchorPrepareViewDefine.AnchorPrepareViewListener;
import com.trtc.uikit.livekit.features.anchorprepare.AnchorPrepareViewDefine.PrepareState;
import com.trtc.uikit.livekit.features.anchorprepare.state.AnchorPrepareConfig;
import com.trtc.uikit.livekit.features.anchorprepare.state.AnchorPrepareState;
import com.trtc.uikit.livekit.features.anchorprepare.state.mediator.PrepareStateMediator;

import io.trtc.tuikit.atomicxcore.api.LiveCoreView;

public class AnchorPrepareManager {
    private final LiveKitLogger                LOGGER = LiveKitLogger.getFeaturesLogger("AnchorPrepareManager");
    private final LiveCoreView                 mCoreView;
    private final AnchorPrepareState           mInternalState;
    private final PrepareStateMediator         mPrepareStateMediator;
    private final PrepareState                 mExternalState;
    private final AnchorPrepareListenerManager mListenerManager;

    public AnchorPrepareManager(LiveCoreView coreView) {
        mCoreView = coreView;
        mInternalState = new AnchorPrepareState();
        mInternalState.selfUserId = TUILogin.getUserId();
        mInternalState.selfUserName = TUILogin.getNickName();

        mExternalState = new PrepareState();
        mPrepareStateMediator = new PrepareStateMediator(mInternalState);
        mListenerManager = new AnchorPrepareListenerManager();
        initPrepareState();
    }

    public AnchorPrepareState getState() {
        return mInternalState;
    }

    public void setCoverURL(String coverURL) {
        mInternalState.coverURL.setValue(coverURL);
    }

    public void setRoomName(String roomName) {
        mInternalState.roomName.setValue(roomName);
    }

    public void setLiveMode(AnchorPrepareViewDefine.LiveStreamPrivacyStatus value) {
        mInternalState.liveMode.setValue(value);
    }

    public void setCoGuestTemplate(int template) {
        mInternalState.coGuestTemplateId.setValue(template);
    }

    public void setCoHostTemplate(int template) {
        mInternalState.coHostTemplateId.setValue(template);
    }

    public void startLive() {
        mListenerManager.notifyAnchorPrepareViewListener(AnchorPrepareViewListener::onClickStartButton);
    }

    public String getDefaultRoomName() {
        if (TextUtils.isEmpty(mInternalState.selfUserName)) {
            return mInternalState.selfUserId;
        } else {
            return mInternalState.selfUserName;
        }
    }

    public LiveCoreView getCoreView() {
        return mCoreView;
    }

    public void destroy() {
        mPrepareStateMediator.destroy();
        mListenerManager.clearAnchorPrepareViewListeners();
    }

    public AnchorPrepareViewDefine.PrepareState getExternalState() {
        return mExternalState;
    }

    private void initPrepareState() {
        mExternalState.coverURL = mPrepareStateMediator.coverURL;
        mExternalState.liveMode = mPrepareStateMediator.liveMode;
        mExternalState.roomName = mPrepareStateMediator.roomName;
        mExternalState.coGuestTemplateId = mPrepareStateMediator.coGuestTemplateId;
        mExternalState.coHostTemplateId = mPrepareStateMediator.coHostTemplateId;
    }

    public void startPreview(TUIRoomDefine.ActionCallback callback) {
        LOGGER.info("requestCameraPermissions:[]");
        PermissionRequest.requestCameraPermissions(ContextProvider.getApplicationContext(), new PermissionCallback() {
            @Override
            public void onRequesting() {
                LOGGER.info("requestCameraPermissions:[onRequesting]");
            }

            @Override
            public void onGranted() {
                LOGGER.info("requestCameraPermissions:[onGranted]");
                mCoreView.startCamera(Boolean.TRUE.equals(mInternalState.useFrontCamera.getValue()),
                        new TUIRoomDefine.ActionCallback() {
                            @Override
                            public void onSuccess() {
                                LOGGER.info("startCamera success, requestMicrophonePermissions");
                                PermissionRequest.requestMicrophonePermissions(ContextProvider.getApplicationContext(), new PermissionCallback() {
                                    @Override
                                    public void onGranted() {
                                        LOGGER.info("requestMicrophonePermissions success");
                                        mCoreView.startMicrophone(new TUIRoomDefine.ActionCallback() {
                                            @Override
                                            public void onSuccess() {
                                                if (callback != null) {
                                                    callback.onSuccess();
                                                }
                                            }

                                            @Override
                                            public void onError(TUICommonDefine.Error error, String message) {
                                                LOGGER.error("startMicrophone failed:error:" + error + ",message:" + message);
                                                if (callback != null) {
                                                    callback.onError(error, message);
                                                }
                                            }
                                        });
                                    }

                                    @Override
                                    public void onDenied() {
                                        LOGGER.error("requestCameraPermissions:[onDenied]");
                                        if (callback != null) {
                                            callback.onError(TUICommonDefine.Error.CAMERA_NOT_AUTHORIZED,
                                                    "requestCameraPermissions:[onDenied"
                                                    + "]");
                                        }
                                    }
                                });
                            }

                            @Override
                            public void onError(TUICommonDefine.Error error, String message) {
                                LOGGER.error("startCamera failed:error:" + error + ",message:" + message);
                                if (callback != null) {
                                    callback.onError(error, message);
                                }

                            }
                        });
            }

            @Override
            public void onDenied() {
                LOGGER.error("requestCameraPermissions:[onDenied]");
                if (callback != null) {
                    callback.onError(TUICommonDefine.Error.CAMERA_NOT_AUTHORIZED, "requestCameraPermissions:[onDenied"
                            + "]");
                }
            }
        });


    }

    public void stopPreview() {
        mCoreView.stopCamera();
        mCoreView.stopMicrophone();
        mListenerManager.notifyAnchorPrepareViewListener(AnchorPrepareViewListener::onClickBackButton);
    }

    public static void disableFeatureMenu(boolean enable) {
        if (Boolean.TRUE.equals(AnchorPrepareConfig.disableFeatureMenu.getValue()) == enable) {
            return;
        }
        AnchorPrepareConfig.disableFeatureMenu.setValue(enable);
    }

    public static void disableMenuSwitchButton(boolean enable) {
        if (Boolean.TRUE.equals(AnchorPrepareConfig.disableMenuSwitchButton.getValue()) == enable) {
            return;
        }
        AnchorPrepareConfig.disableMenuSwitchButton.setValue(enable);
    }

    public static void disableMenuBeautyButton(boolean enable) {
        if (Boolean.TRUE.equals(AnchorPrepareConfig.disableMenuBeautyButton.getValue()) == enable) {
            return;
        }
        AnchorPrepareConfig.disableMenuBeautyButton.setValue(enable);
    }

    public static void disableMenuAudioEffectButton(boolean enable) {
        if (Boolean.TRUE.equals(AnchorPrepareConfig.disableMenuAudioEffectButton.getValue()) == enable) {
            return;
        }
        AnchorPrepareConfig.disableMenuAudioEffectButton.setValue(enable);
    }

    public void addAnchorPrepareViewListener(AnchorPrepareViewDefine.AnchorPrepareViewListener listener) {
        mListenerManager.addAnchorPrepareViewListener(listener);
    }

    public void removeAnchorPrepareViewListener(AnchorPrepareViewDefine.AnchorPrepareViewListener listener) {
        mListenerManager.removeAnchorPrepareViewListener(listener);
    }
}
