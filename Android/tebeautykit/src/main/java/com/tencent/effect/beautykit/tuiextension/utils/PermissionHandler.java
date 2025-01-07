package com.tencent.effect.beautykit.tuiextension.utils;

import android.Manifest;
import android.app.Activity;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.util.Log;
import android.widget.Toast;

import androidx.annotation.Keep;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public abstract class PermissionHandler {

    private static final String TAG = "PermissionHandler";

    private Activity mActivity;
    private int mRequestCode;

    public PermissionHandler(Activity activity) {
        mActivity = activity;
    }

    public void start() {
        String[] permissions = onGetPermissions();
        List<String> checkPermission = new ArrayList<>();
        for (String permission:permissions){
            if(Manifest.permission.SYSTEM_ALERT_WINDOW.equals(permission)){
                continue;
            }
            checkPermission.add(permission);
        }
        start(checkPermission.toArray(new String[0]));
    }

    public void start(String[] permissions) {
        String[] no = getNoGrantPermissions(permissions);
        if (no == null || no.length == 0) {
            onAllPermissionGranted();
        } else {
            doRequestPermissions(no);
        }
    }

    public void onRequestPermissionsResult(int requestCode, String permissions[], int[] grantResults) {
        List<String> declinedPermissions = new ArrayList<>();
        if (requestCode == mRequestCode) {
            for (int i = 0; i < permissions.length; i++) {
                if (grantResults[i] != PackageManager.PERMISSION_GRANTED) {
                    if (!shouldIgnore(permissions[i])) {
                        declinedPermissions.add(permissions[i]);
                    }
                }
            }
            int declinedPermissionsCount = declinedPermissions.size();
            if (declinedPermissionsCount == 0) {
                onAllPermissionGranted();
            } else {
                onPermissionsDecline(declinedPermissions.toArray(new String[declinedPermissionsCount]));
            }
        }
    }

    protected boolean shouldIgnore(String permission) {
        return false;
    };


    @Keep
    protected String[] onGetPermissions() {
        PackageInfo info = null;
        try {
            info = mActivity.getPackageManager().getPackageInfo(mActivity.getPackageName(), PackageManager.GET_PERMISSIONS);
        } catch (NameNotFoundException e) {
            e.printStackTrace();// not possible
        }
        if (info.requestedPermissions == null) {
            Log.w(TAG, "android.content.pm.PackageInfo.requestedPermissions == null, this app does not require any permissions?");
        }
        return info.requestedPermissions;
    }

    @Keep
    protected void onPermissionsDecline(String[] permissions){
        String msg = "permission declined: " + Arrays.toString(permissions);
        Log.w(TAG, msg);
        Toast.makeText(mActivity, msg, Toast.LENGTH_LONG).show();
    }

    @Keep
    protected abstract void onAllPermissionGranted();

    private String[] getNoGrantPermissions(String[] permissions) {
        if (permissions == null) {
            return new String[0];
        }

        List<String> missingPermissions = new ArrayList<>();
        for (String p : permissions) {
            if (!hasPermission(p)) {
                missingPermissions.add(p);
            }
        }
        return missingPermissions.toArray(new String[missingPermissions.size()]);
    }

    private boolean hasPermission(String permission) {
        return ContextCompat.checkSelfPermission(mActivity, permission) == PackageManager.PERMISSION_GRANTED;
    }

    private void doRequestPermissions(String[] permissions) {
        mRequestCode = 1024;
        ActivityCompat.requestPermissions(mActivity, permissions, mRequestCode);
    }

}
