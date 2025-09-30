package com.trtc.uikit.livekit.common;

import android.Manifest;
import android.content.Context;
import android.content.pm.ApplicationInfo;

import com.trtc.tuikit.common.permission.PermissionCallback;
import com.trtc.tuikit.common.permission.PermissionRequester;
import com.trtc.uikit.livekit.R;

import java.util.ArrayList;
import java.util.List;

public class PermissionRequest {

    public static void requestMicrophonePermissions(Context context, PermissionCallback callback) {
        StringBuilder title = new StringBuilder().append(context.getString(R.string.common_permission_microphone));
        StringBuilder reason = new StringBuilder();
        reason.append(context.getString(R.string.common_permission_mic_reason));
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
                .title(context.getString(R.string.common_permission_title, appName, title))
                .description(reason.toString())
                .settingsTip(context.getString(R.string.common_permission_tips, title) + "\n" + reason)
                .callback(permissionCallback)
                .request();
    }

    public static void requestCameraPermissions(Context context, PermissionCallback callback) {
        StringBuilder title = new StringBuilder().append(context.getString(R.string.common_permission_camera));
        StringBuilder reason = new StringBuilder();
        reason.append(context.getString(R.string.common_permission_camera_reason));
        List<String> permissionList = new ArrayList<>();
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
                .title(context.getString(R.string.common_permission_title, appName, title))
                .description(reason.toString())
                .settingsTip(context.getString(R.string.common_permission_tips, title) + "\n" + reason)
                .callback(permissionCallback)
                .request();
    }
}
