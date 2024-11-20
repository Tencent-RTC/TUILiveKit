package com.trtc.uikit.livekit.voiceroomcore.manager.module;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUIConfig;
import com.tencent.qcloud.tuicore.permission.PermissionCallback;
import com.trtc.uikit.livekit.voiceroomcore.common.utils.Logger;
import com.trtc.uikit.livekit.voiceroomcore.common.utils.PermissionRequest;
import com.trtc.uikit.livekit.voiceroomcore.manager.observer.SeatGridViewObserverManager;
import com.trtc.uikit.livekit.voiceroomcore.manager.api.IVoiceRoomService;
import com.trtc.uikit.livekit.voiceroomcore.state.VoiceRoomState;

public class MediaManager extends BaseManager {
    private static final String FILE = "MediaManager";

    public MediaManager(VoiceRoomState state, IVoiceRoomService service, SeatGridViewObserverManager observerManager) {
        super(state, service, observerManager);
    }

    @Override
    public void destroy() {

    }

    public void startMicrophone(TUIRoomDefine.ActionCallback callback) {
        boolean hasMicrophonePermission = mMediaState.hasMicrophonePermission;
        if (hasMicrophonePermission) {
            openLocalMicrophoneByService(callback);
            return;
        }
        Logger.info(FILE, "requestMicrophonePermissions:[]");
        PermissionRequest.requestMicrophonePermissions(TUIConfig.getAppContext(), new PermissionCallback() {
            @Override
            public void onRequesting() {
                Logger.info(FILE, " requestMicrophonePermissions:[onRequesting}");
            }

            @Override
            public void onGranted() {
                Logger.info(FILE, " requestMicrophonePermissions:[onGranted]");
                mMediaState.hasMicrophonePermission = true;
                openLocalMicrophoneByService(callback);
            }

            @Override
            public void onDenied() {
                Logger.warn(FILE, " requestMicrophonePermissions:[onDenied]");
                if (callback != null) {
                    callback.onError(TUICommonDefine.Error.PERMISSION_DENIED, "requestMicrophonePermissions:[onDenied"
                            + "]");
                }
            }
        });
    }

    public void stopMicrophone() {
        mService.closeLocalMicrophone();
    }

    public void muteMicrophone() {
        mService.muteLocalAudio();
    }

    public void unMuteLocalAudio(TUIRoomDefine.ActionCallback callback) {
        mService.unMuteLocalAudio(callback);
    }

    private void openLocalMicrophoneByService(TUIRoomDefine.ActionCallback callback) {
        mService.openLocalMicrophone(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mMediaState.isMicrophoneOpened = true;
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE,"openLocalMicrophone, error: " + error + ", message: " + message);
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }
}
