package com.tencent.cloud.tuikit.flutter.tuilivekit.tuilivekit;

import android.util.Log;

import androidx.annotation.NonNull;

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

    private MethodChannel channel;

    @Override
    public void onAttachedToEngine(@NonNull FlutterPluginBinding flutterPluginBinding) {
        channel = new MethodChannel(flutterPluginBinding.getBinaryMessenger(), "tuilivekit");
        channel.setMethodCallHandler(this);
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
        channel.setMethodCallHandler(null);
    }

    public void apiLog(MethodCall call, MethodChannel.Result result) {
        String logString = MethodUtils.getMethodRequiredParams(call, "logString", result);
        int level = MethodUtils.getMethodRequiredParams(call, "level", result);

        switch (level) {
            case 1:
                Log.w(TAG, logString);
                break;
            case 2:
                Log.e(TAG, logString);
                break;
            default:
                Log.i(TAG, logString);
                break;
        }
        result.success(0);
    }
}
