package com.trtc.uikit.livekit.features.anchorprepare.manager;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorprepare.AnchorPrepareViewDefine;
import com.trtc.uikit.livekit.features.anchorprepare.AnchorPrepareViewDefine.PrepareState;
import com.trtc.uikit.livekit.features.anchorprepare.state.AnchorPrepareConfig;
import com.trtc.uikit.livekit.features.anchorprepare.state.AnchorPrepareState;
import com.trtc.uikit.livekit.features.anchorprepare.state.mediator.PrepareStateMediator;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

public class AnchorPrepareManager {
    private final LiveKitLogger        LOGGER = LiveKitLogger.getFeaturesLogger("AnchorPrepareManager");
    private final LiveCoreView         mCoreView;
    private final AnchorPrepareState   mInternalState;
    private final PrepareStateMediator mPrepareStateMediator;
    private final PrepareState         mExternalState;

    public AnchorPrepareManager(LiveCoreView coreView) {
        mCoreView = coreView;
        mInternalState = new AnchorPrepareState();
        mInternalState.selfUserId = TUILogin.getUserId();
        mInternalState.selfUserName = TUILogin.getNickName();

        mExternalState = new PrepareState();
        mPrepareStateMediator = new PrepareStateMediator(mInternalState);
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

    public void startLive() {
        mInternalState.startLiveClick.setValue(true);
    }

    public String getDefaultRoomName() {
        if (TextUtils.isEmpty(mInternalState.selfUserName)) {
            return mInternalState.selfUserId;
        } else {
            return mInternalState.selfUserName;
        }
    }

    public void destroy() {
        mPrepareStateMediator.destroy();
    }

    public AnchorPrepareViewDefine.PrepareState getExternalState() {
        return mExternalState;
    }

    private void initPrepareState() {
        mExternalState.coverURL = mPrepareStateMediator.coverURL;
        mExternalState.liveMode = mPrepareStateMediator.liveMode;
        mExternalState.roomName = mPrepareStateMediator.roomName;
        mExternalState.startLiveClick = mInternalState.startLiveClick;
    }

    public void startPreview() {
        mCoreView.startCamera(Boolean.TRUE.equals(mInternalState.useFrontCamera.getValue()),
                new TUIRoomDefine.ActionCallback() {
                    @Override
                    public void onSuccess() {
                        LOGGER.info("startCamera success");
                        mCoreView.startMicrophone(new TUIRoomDefine.ActionCallback() {
                            @Override
                            public void onSuccess() {
                                LOGGER.info("startMicrophone success");
                            }

                            @Override
                            public void onError(TUICommonDefine.Error error, String message) {
                                LOGGER.error("startMicrophone failed:error:" + error + ",message:" + message);
                            }
                        });
                    }

                    @Override
                    public void onError(TUICommonDefine.Error error, String message) {
                        LOGGER.error("startCamera failed:error:" + error + ",message:" + message);
                    }
                });
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
}
