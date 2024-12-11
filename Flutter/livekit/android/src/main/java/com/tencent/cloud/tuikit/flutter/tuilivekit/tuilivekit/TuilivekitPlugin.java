package com.tencent.cloud.tuikit.flutter.tuilivekit.tuilivekit;

import android.content.Context;
import android.util.Log;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.flutter.tuilivekit.tuilivekit.utils.LiveKitLog;
import com.tencent.cloud.tuikit.flutter.tuilivekit.tuilivekit.utils.MethodUtils;

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
    private Context       mContext;

    @Override
    public void onAttachedToEngine(@NonNull FlutterPluginBinding flutterPluginBinding) {
        mContext = flutterPluginBinding.getApplicationContext();
        mMethodChannel = new MethodChannel(flutterPluginBinding.getBinaryMessenger(), "tuilivekit");
        mMethodChannel.setMethodCallHandler(this);
    }

    @Override
    public void onMethodCall(@NonNull MethodCall call, @NonNull Result result) {
        try {
            Method method = TuilivekitPlugin.class.getDeclaredMethod(call.method, MethodCall.class,
                    MethodChannel.Result.class);
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

        switch (level) {
            case 1:
                LiveKitLog.warn(mContext, logString);
                break;
            case 2:
                LiveKitLog.error(mContext, logString);
                break;
            default:
                LiveKitLog.info(mContext, logString);
                break;
        }
        result.success(0);
    }
}
