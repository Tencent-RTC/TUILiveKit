package com.tencent.cloud.tuikit.flutter.tuilivekit.utils;

import static android.content.Context.APP_OPS_SERVICE;

import android.app.Activity;
import android.app.AppOpsManager;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Build;
import android.provider.Settings;

public class SettingsManager {
    private final Activity mActivity;

    public SettingsManager(Activity activity) {
        mActivity = activity;
    }

    public void openWifiSettings() {
        Intent intent = new Intent(Settings.ACTION_WIFI_SETTINGS);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        mActivity.startActivity(intent);
    }

    public void openAppSettings() {
        Intent intent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
        Uri uri = Uri.fromParts("package", mActivity.getPackageName(), null);
        intent.setData(uri);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        mActivity.startActivity(intent);
    }

    public void checkAndOpenPipSettings(Activity activity) {
        if (activity == null || activity.isDestroyed() || activity.isFinishing()) {
            return;
        }
        if (!hasPipPermission(activity)) {
            Intent intent = new Intent("android.settings.PICTURE_IN_PICTURE_SETTINGS",
                    Uri.parse("package:" + activity.getPackageName()));
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            activity.startActivity(intent);
        }
    }

    public boolean hasPipPermission(Activity activity) {
        if (activity == null || activity.isDestroyed() || activity.isFinishing()) {
            return false;
        }
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O && activity.getPackageManager().hasSystemFeature(PackageManager.FEATURE_PICTURE_IN_PICTURE)) {
            AppOpsManager appOpsManager = (AppOpsManager) activity.getSystemService(APP_OPS_SERVICE);
            return AppOpsManager.MODE_ALLOWED == appOpsManager.checkOpNoThrow(AppOpsManager.OPSTR_PICTURE_IN_PICTURE,
                    activity.getApplicationInfo().uid, activity.getPackageName());
        }
        return false;
    }
}