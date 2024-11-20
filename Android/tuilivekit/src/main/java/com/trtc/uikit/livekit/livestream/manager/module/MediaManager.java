package com.trtc.uikit.livekit.livestream.manager.module;

import static com.tencent.liteav.beauty.TXBeautyManager.TXBeautyStyleSmooth;

import android.Manifest;
import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.text.TextUtils;
import android.view.View;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUIConfig;
import com.tencent.qcloud.tuicore.TUIConstants;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.permission.PermissionCallback;
import com.tencent.qcloud.tuicore.permission.PermissionRequester;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.beauty.TEBeautyService;
import com.trtc.uikit.livekit.livestream.manager.api.ILiveService;
import com.trtc.uikit.livekit.livestream.manager.api.LiveStreamLog;
import com.trtc.uikit.livekit.livestream.state.LiveState;

import java.util.ArrayList;
import java.util.List;

public class MediaManager extends BaseManager {
    private static final String          TAG = "MediaManager";
    private final        TEBeautyService mTEBeautyService;

    public MediaManager(LiveState state, ILiveService service) {
        super(state, service);
        mTEBeautyService = new TEBeautyService(state, service.getTRTCCloud());
    }

    @Override
    public void destroy() {
        LiveStreamLog.info(TAG + " destroy");
        mTEBeautyService.clearBeautyView();
    }

    public void setLocalVideoView(TUIVideoView view) {
        mLiveService.setLocalVideoView(view);
    }

    public void openLocalCamera() {
        boolean hasCameraPermission = mMediaState.hasCameraPermission.get();
        if (hasCameraPermission) {
            openLocalCameraByService();
            return;
        }
        LiveStreamLog.info(TAG + " requestPermissions:[]");
        requestPermissions(TUIConfig.getAppContext(), new PermissionCallback() {
            @Override
            public void onRequesting() {
                LiveStreamLog.info(TAG + " requestPermissions:[onRequesting]");
            }

            @Override
            public void onGranted() {
                LiveStreamLog.info(TAG + " requestPermissions:[onGranted]");
                mMediaState.hasCameraPermission.set(true);
                openLocalCameraByService();
            }

            @Override
            public void onDenied() {
                LiveStreamLog.error(TAG + " requestPermissions:[onDenied]");
            }
        });
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
        mTEBeautyService.initBeautyKit();
    }

    public View getTEBeautyView(Context context) {
        return mTEBeautyService.getBeautyView(context);
    }

    public void clearEBeautyView() {
        mTEBeautyService.clearBeautyView();
    }

    private void openLocalCameraByService() {
        if (mMediaState.isCameraOpened.get()) {
            LiveStreamLog.error(TAG + " camera is opened, no need to open again");
            return;
        }
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
    
    private void requestPermissions(Context context, PermissionCallback callback) {
        StringBuilder title = new StringBuilder().append(context.getString(R.string.livekit_permission_microphone));
        StringBuilder reason = new StringBuilder();
        String microphonePermissionsDescription = (String) TUICore.createObject(
                TUIConstants.Privacy.PermissionsFactory.FACTORY_NAME,
                TUIConstants.Privacy.PermissionsFactory.PermissionsName.MICROPHONE_PERMISSIONS, null);
        if (!TextUtils.isEmpty(microphonePermissionsDescription)) {
            reason.append(microphonePermissionsDescription);
        } else {
            reason.append(context.getString(R.string.livekit_permission_mic_reason));
        }
        List<String> permissionList = new ArrayList<>();
        permissionList.add(Manifest.permission.RECORD_AUDIO);

        title.append(context.getString(R.string.livekit_permission_separator));
        title.append(context.getString(R.string.livekit_permission_camera));
        String cameraPermissionsDescription = (String) TUICore.createObject(
                TUIConstants.Privacy.PermissionsFactory.FACTORY_NAME,
                TUIConstants.Privacy.PermissionsFactory.PermissionsName.CAMERA_PERMISSIONS, null);
        if (!TextUtils.isEmpty(cameraPermissionsDescription)) {
            reason.append(cameraPermissionsDescription);
        } else {
            reason.append(context.getString(R.string.livekit_permission_camera_reason));
        }
        permissionList.add(Manifest.permission.CAMERA);

        PermissionCallback permissionCallback = new PermissionCallback() {
            @Override
            public void onGranted() {
                if (callback != null) {
                    callback.onGranted();
                }
            }

            @Override
            public void onDenied() {
                super.onDenied();
                if (callback != null) {
                    callback.onDenied();
                }
            }
        };

        ApplicationInfo applicationInfo = context.getApplicationInfo();
        String appName = context.getPackageManager().getApplicationLabel(applicationInfo).toString();

        String[] permissions = permissionList.toArray(new String[0]);
        PermissionRequester.newInstance(permissions)
                .title(context.getString(R.string.livekit_permission_title, appName, title))
                .description(reason.toString())
                .settingsTip(context.getString(R.string.livekit_permission_tips, title) + "\n" + reason)
                .callback(permissionCallback)
                .request();
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
}
