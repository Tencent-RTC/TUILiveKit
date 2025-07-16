package com.tencent.cloud.tuikit.flutter.tuilivekit;

import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.util.Log;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.flutter.tuilivekit.utils.LiveKitLog;
import com.tencent.cloud.tuikit.flutter.tuilivekit.utils.MethodUtils;
import com.trtc.tuikit.common.foregroundservice.AudioForegroundService;
import com.trtc.tuikit.common.foregroundservice.MediaForegroundService;
import com.trtc.tuikit.common.foregroundservice.VideoForegroundService;

import java.lang.reflect.Method;

import io.flutter.embedding.engine.plugins.FlutterPlugin;
import io.flutter.plugin.common.MethodCall;
import io.flutter.plugin.common.MethodChannel;
import io.flutter.plugin.common.MethodChannel.MethodCallHandler;
import io.flutter.plugin.common.MethodChannel.Result;

/**
 * TuilivekitPlugin
 */
public class TuilivekitPlugin implements FlutterPlugin, MethodCallHandler {

    public static final String TAG = "TuilivekitPlugin";

    private MethodChannel mMethodChannel;
    private Context mContext;

    @Override
    public void onAttachedToEngine(@NonNull FlutterPluginBinding flutterPluginBinding) {
        mContext = flutterPluginBinding.getApplicationContext();
        mMethodChannel = new MethodChannel(flutterPluginBinding.getBinaryMessenger(), "tuilivekit");
        mMethodChannel.setMethodCallHandler(this);
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

    @Override
    public void onDetachedFromEngine(@NonNull FlutterPluginBinding binding) {
        mMethodChannel.setMethodCallHandler(null);
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
}
