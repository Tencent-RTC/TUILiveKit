package com.tencent.effect.beautykit.tuiextension;

import android.content.Context;
import android.content.SharedPreferences;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Log;

import com.tencent.effect.beautykit.TEBeautyKit;
import com.tencent.effect.beautykit.config.TEUIConfig;
import com.tencent.qcloud.tuicore.interfaces.TUIServiceCallback;
import com.tencent.xmagic.telicense.TELicenseCheck;

import java.io.File;

public final class TEBeautySettings {
    private static final String TAG = "TEBeautySettings";

    private static final String BEAUTY_FILE_DIR_NAME  = "xmagic";
    private static final String KEY_TEBEAUTY_SETTINGS = "tebeauty_settings";
    private static final String KEY_RESOURCE_COPIED   = "resource_copied";

    private String mAppVersionName = "";

    private static final TEBeautySettings INSTANCE = new TEBeautySettings();

    public enum BeautyVersion {
        A1_00, A1_01, A1_02, A1_03, A1_04, A1_05, A1_06, S1_00, S1_01, S1_02, S1_03, S1_04, S1_05, S1_06, S1_07
    }

    public static TEBeautySettings getInstance() {
        return INSTANCE;
    }

    private TEBeautySettings() {}

    public void initBeautySettings(Context context, BeautyVersion beautyVersion,
                                   String licenseUrl, String licenseKey) {
        if (context == null) {
            Log.e(TAG, "context is null");
            return;
        }
        Context appContext = context.getApplicationContext();
        copyBeautyResources(appContext, new TUIServiceCallback() {
            @Override
            public void onServiceCallback(int errorCode, String errorMessage, Bundle bundle) {
                Log.i(TAG, "copyRes callback: " + errorCode + ", " + errorMessage);
                setBeautyVersion(beautyVersion);
                TEBeautyKit.setTELicense(appContext, licenseUrl, licenseKey, (error, msg) -> {
                    Log.i(TAG, "setTELicense callback: " + error + ", " + msg);
                    if (error == TELicenseCheck.ERROR_OK) {
                        Log.i(TAG, "License Verification Success");
                    } else {
                        Log.e(TAG, "License Verification Failed: " + error + ", " + msg);
                    }
                });
            }
        });
    }

    private void copyBeautyResources(Context context, TUIServiceCallback callback) {
        Context appContext = context.getApplicationContext();
        String resPath = new File(appContext.getFilesDir(), getBeautyFileDirName()).getAbsolutePath();
        TEBeautyKit.setResPath(resPath);
        if (isNeedCopyRes(appContext)) {
            new Thread(() -> {
                TEBeautyKit.copyRes(appContext);
                saveCopyData(context);
                if (callback != null) {
                    callback.onServiceCallback(0, null, null);
                }
            }).start();
        } else {
            if (callback != null) {
                callback.onServiceCallback(0, null, null);
            }
        }
    }

    private void setBeautyVersion(BeautyVersion version) {
        String beautyVersion = version.name();
        TEUIConfig.getInstance().setTEPanelViewRes(
                "beauty_panel/" + beautyVersion + "/beauty.json",
                "beauty_panel/" + beautyVersion + "/beauty_body.json",
                "beauty_panel/" + beautyVersion + "/lut.json",
                "beauty_panel/" + beautyVersion + "/motions.json",
                "beauty_panel/" + beautyVersion + "/makeup.json",
                "beauty_panel/" + beautyVersion + "/segmentation.json"
        );
    }

    private String getBeautyFileDirName() {
        return BEAUTY_FILE_DIR_NAME;
    }

    private String getAppVersionName(Context context) {
        if (!TextUtils.isEmpty(mAppVersionName)) {
            return mAppVersionName;
        }
        String versionName = "";
        try {
            PackageManager pm = context.getPackageManager();
            PackageInfo pi = pm.getPackageInfo(context.getPackageName(), 0);
            versionName = pi.versionName;
            if (TextUtils.isEmpty(versionName)) {
                return "";
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        mAppVersionName = versionName;
        return versionName;
    }

    private boolean isNeedCopyRes(Context context) {
        String appVersionName = getAppVersionName(context);
        SharedPreferences sp = context.getSharedPreferences(KEY_TEBEAUTY_SETTINGS, Context.MODE_PRIVATE);
        String savedVersionName = sp.getString(KEY_RESOURCE_COPIED, "");
        return !TextUtils.equals(savedVersionName, appVersionName);
    }

    private void saveCopyData(Context context) {
        String appVersionName = getAppVersionName(context);
        SharedPreferences sp = context.getSharedPreferences(KEY_TEBEAUTY_SETTINGS, Context.MODE_PRIVATE);
        sp.edit().putString(KEY_RESOURCE_COPIED, appVersionName).commit();
    }
}

