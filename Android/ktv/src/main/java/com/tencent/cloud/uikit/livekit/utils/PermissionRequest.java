package com.tencent.cloud.uikit.livekit.utils;

import android.Manifest;
import android.content.Context;
import android.content.pm.ApplicationInfo;

import com.tencent.cloud.uikit.ktv.R;
import com.trtc.tuikit.common.permission.PermissionCallback;
import com.trtc.tuikit.common.permission.PermissionRequester;

import java.util.ArrayList;
import java.util.List;

public class PermissionRequest {

    public static void requestMicrophonePermissions(Context context, PermissionCallback callback) {
        StringBuilder title = new StringBuilder().append(context.getString(R.string.ktv_permission_microphone));
        StringBuilder reason = new StringBuilder();
        reason.append(context.getString(R.string.ktv_permission_mic_reason));
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
                .title(context.getString(R.string.ktv_permission_title, appName, title))
                .description(reason.toString())
                .settingsTip(context.getString(R.string.ktv_permission_tips, title) + "\n" + reason)
                .callback(permissionCallback)
                .request();
    }
}
