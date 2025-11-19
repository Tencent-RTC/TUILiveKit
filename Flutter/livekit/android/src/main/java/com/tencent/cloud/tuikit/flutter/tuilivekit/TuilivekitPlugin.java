package com.tencent.cloud.tuikit.flutter.tuilivekit;

import android.app.Activity;
import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.util.Log;
import android.view.Window;
import android.view.WindowManager;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.flutter.tuilivekit.utils.LiveKitLog;
import com.tencent.cloud.tuikit.flutter.tuilivekit.utils.MethodUtils;
import com.tencent.cloud.tuikit.flutter.tuilivekit.utils.SettingsManager;
import com.tencent.cloud.tuikit.flutter.tuilivekit.utils.ThermalManager;
import com.tencent.cloud.tuikit.flutter.tuilivekit.utils.NetworkManager;
import com.trtc.tuikit.common.foregroundservice.AudioForegroundService;
import com.trtc.tuikit.common.foregroundservice.MediaForegroundService;
import com.trtc.tuikit.common.foregroundservice.VideoForegroundService;

import java.lang.reflect.Method;

import io.flutter.embedding.engine.plugins.FlutterPlugin;
import io.flutter.embedding.engine.plugins.activity.ActivityAware;
import io.flutter.embedding.engine.plugins.activity.ActivityPluginBinding;
import io.flutter.plugin.common.EventChannel;
import io.flutter.plugin.common.MethodCall;
import io.flutter.plugin.common.MethodChannel;
import io.flutter.plugin.common.MethodChannel.MethodCallHandler;
import io.flutter.plugin.common.MethodChannel.Result;

/**
 * TuilivekitPlugin
 */
public class TuilivekitPlugin implements FlutterPlugin, MethodCallHandler, ActivityAware {

    public static final String TAG = "TuilivekitPlugin";

    private MethodChannel mMethodChannel;
    private EventChannel mThermalEventChannel;
    private EventChannel mNetworkEventChannel;
    private Context mContext;
    private Activity mActivity;
    private ThermalManager mThermalManager;
    private SettingsManager mSettingsManager;
    private NetworkManager mNetworkManager;


    @Override
    public void onAttachedToEngine(@NonNull FlutterPluginBinding flutterPluginBinding) {
        mContext = flutterPluginBinding.getApplicationContext();
        mMethodChannel = new MethodChannel(flutterPluginBinding.getBinaryMessenger(), "tuilivekit");
        mMethodChannel.setMethodCallHandler(this);

        mThermalManager = new ThermalManager(mContext);
        mThermalEventChannel = new EventChannel(flutterPluginBinding.getBinaryMessenger(), "tuilivekit_thermal_events");
        mThermalEventChannel.setStreamHandler(mThermalManager);

        mNetworkManager = new NetworkManager(mContext);
        mNetworkEventChannel = new EventChannel(flutterPluginBinding.getBinaryMessenger(), "tuilivekit_network_events");
        mNetworkEventChannel.setStreamHandler(mNetworkManager);
    }

    @Override
    public void onDetachedFromEngine(@NonNull FlutterPluginBinding binding) {
        mMethodChannel.setMethodCallHandler(null);
        mThermalEventChannel.setStreamHandler(null);
        mNetworkEventChannel.setStreamHandler(null);
    }

    @Override
    public void onAttachedToActivity(@NonNull ActivityPluginBinding binding) {
        mActivity = binding.getActivity();
        mSettingsManager = new SettingsManager(mActivity);
    }

    @Override
    public void onDetachedFromActivityForConfigChanges() {
        mActivity = null;
    }

    @Override
    public void onReattachedToActivityForConfigChanges(@NonNull ActivityPluginBinding binding) {
        mActivity = binding.getActivity();
    }

    @Override
    public void onDetachedFromActivity() {
        mActivity = null;
    }

    @Override
    public void onMethodCall(@NonNull MethodCall call, @NonNull Result result) {
        try {
            Method method = TuilivekitPlugin.class.getDeclaredMethod(call.method, MethodCall.class, MethodChannel.Result.class);
            method.invoke(this, call, result);
        } catch (Exception e) {
            Log.e(TAG, "onMethodCall |method=" + call.method + "|arguments=" + call.arguments + "|error=" + e);
        }
    }

    public void apiLog(MethodCall call, MethodChannel.Result result) {
        String logString = MethodUtils.getMethodRequiredParams(call, "logString", result);
        int level = MethodUtils.getMethodRequiredParams(call, "level", result);
        String module = MethodUtils.getMethodRequiredParams(call, "module", result);
        String file = MethodUtils.getMethodRequiredParams(call, "file", result);
        int line = MethodUtils.getMethodRequiredParams(call, "line", result);

        switch (level) {
            case 1:
                LiveKitLog.warn(mContext, module, file, line, logString);
                break;
            case 2:
                LiveKitLog.error(mContext, module, file, line, logString);
                break;
            default:
                LiveKitLog.info(mContext, module, file, line, logString);
                break;
        }
        result.success(0);
    }

    public void startForegroundService(MethodCall call, MethodChannel.Result result) {
        int level = MethodUtils.getMethodRequiredParams(call, "serviceType", result);
        String description = MethodUtils.getMethodRequiredParams(call, "description", result);
        String title = MethodUtils.getMethodRequiredParams(call, "title", result);
        if (title.isEmpty()) {
            title = getApplicationName();
        }
        switch (level) {
            case 1:
                AudioForegroundService.start(mContext, title, description, 0);
                break;
            case 2:
                MediaForegroundService.start(mContext, title, description, 0);
                break;
            default:
                VideoForegroundService.start(mContext, title, description, 0);
                break;
        }
        result.success(0);
    }

    public void stopForegroundService(MethodCall call, MethodChannel.Result result) {
        int level = MethodUtils.getMethodRequiredParams(call, "serviceType", result);
        switch (level) {
            case 1:
                AudioForegroundService.stop(mContext);
                break;
            case 2:
                MediaForegroundService.stop(mContext);
                break;
            default:
                VideoForegroundService.stop(mContext);
                break;
        }
        result.success(0);
    }

    private String getApplicationName() {
        try {
            PackageManager packageManager = mContext.getPackageManager();
            ApplicationInfo applicationInfo = packageManager.getApplicationInfo(mContext.getPackageName(), 0);
            return (String) packageManager.getApplicationLabel(applicationInfo);
        } catch (Exception e) {
            Log.e(TAG, e.toString());
            return "";
        }
    }

    public void enableWakeLock(MethodCall call, MethodChannel.Result result) {
        boolean enable = MethodUtils.getMethodRequiredParams(call, "enable", result);
        try {
            if (mActivity != null) {
                mActivity.runOnUiThread(() -> {
                    Window window = mActivity.getWindow();
                    if (enable) {
                        window.addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
                        Log.d(TAG, "enableWakeLock: true");
                    } else {
                        window.clearFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
                        Log.d(TAG, "enableWakeLock: false");
                    }
                });
            }
            result.success(0);
        } catch (Exception e) {
            Log.e(TAG, "enableWakeLock error: " + e.getMessage());
            result.error("enableWakeLock error", "Failed to set screen keep-on", e);
        }
    }

    public void openWifiSettings(MethodCall call, MethodChannel.Result result) {
        mSettingsManager.openWifiSettings();
        result.success(null);
    }

    public void openAppSettings(MethodCall call, MethodChannel.Result result) {
        mSettingsManager.openAppSettings();
        result.success(null);
    }

    public void openPipSettings(MethodCall call, MethodChannel.Result result) {
        mSettingsManager.checkAndOpenPipSettings(mActivity);
        result.success(null);
    }

    public void hasPipPermission(MethodCall call, MethodChannel.Result result) {
        result.success(mSettingsManager.hasPipPermission(mActivity));
    }

    public void getCurrentNetworkStatus(MethodCall call, MethodChannel.Result result) {
        result.success(mNetworkManager.getCurrentNetworkState());
    }
}
