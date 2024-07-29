package com.trtc.uikit.livekit.common.utils;

import android.Manifest;
import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.text.TextUtils;

import com.tencent.qcloud.tuicore.TUIConstants;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.permission.PermissionCallback;
import com.tencent.qcloud.tuicore.permission.PermissionRequester;
import com.trtc.uikit.livekit.R;

import java.util.ArrayList;
import java.util.List;

public class PermissionRequest {

    public static void requestMicrophonePermissions(Context context, PermissionCallback callback) {
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

    public static void requestPermissions(Context context, PermissionCallback callback) {
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
}
