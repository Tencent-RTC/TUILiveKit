package com.trtc.uikit.livekit.example.view.crash;

import android.annotation.SuppressLint;
import android.app.Application;
import android.content.Context;
import android.content.Intent;
import android.os.Build;

import androidx.annotation.NonNull;

import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.example.view.login.LoginActivity;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;

public class TUILiveKitCrashHandler implements Thread.UncaughtExceptionHandler {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getCommonLogger("TUILiveKitCrashHandler");
    private              Context       mContext;


    @SuppressLint("StaticFieldLeak")
    private static volatile TUILiveKitCrashHandler INSTANCE;

    private TUILiveKitCrashHandler() {
    }

    public static TUILiveKitCrashHandler getInstance() {
        if (INSTANCE == null) {
            synchronized (TUILiveKitCrashHandler.class) {
                if (INSTANCE == null) {
                    INSTANCE = new TUILiveKitCrashHandler();
                }
            }
        }
        return INSTANCE;
    }

    public void initialize(Application application) {
        this.mContext = application.getApplicationContext();

        Thread.setDefaultUncaughtExceptionHandler(this);
    }

    @Override
    public void uncaughtException(@NonNull Thread thread, @NonNull Throwable exception) {
        String stackTrace = getStackTrace(exception);
        String deviceInfo = collectDeviceInfo();
        String timestamp = System.currentTimeMillis() + "";

        String fullLog = "--------- CRASH REPORT ---------\n" +
                "TIME: " + timestamp + "\n\n" +
                "STACK TRACE:\n" + stackTrace + "\n" +
                "DEVICE INFO:\n" + deviceInfo +
                "\n--------- END ---------";

        LOGGER.error("Application crashed:\n" + fullLog);

        enterLoginActivity();
        System.exit(10);
    }

    private void enterLoginActivity() {
        Intent intent = new Intent(mContext, LoginActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
        mContext.startActivity(intent);
    }

    private String getStackTrace(Throwable ex) {
        Writer writer = new StringWriter();
        PrintWriter printWriter = new PrintWriter(writer);
        ex.printStackTrace(printWriter);
        printWriter.close();
        return writer.toString();
    }

    private String collectDeviceInfo() {
        StringBuilder sb = new StringBuilder();
        try {
            sb.append("App Version: ")
                    .append(mContext.getPackageManager().getPackageInfo(mContext.getPackageName(), 0).versionName)
                    .append(" (")
                    .append(mContext.getPackageManager().getPackageInfo(mContext.getPackageName(), 0).versionCode)
                    .append(")\n");

            sb.append("Device: ").append(Build.MANUFACTURER).append(" ").append(Build.MODEL).append("\n");
            sb.append("Android Version: ").append(Build.VERSION.RELEASE).append(" (API ").append(Build.VERSION.SDK_INT).append(")\n");
            sb.append("CPU ABI: ").append(Build.SUPPORTED_ABIS[0]).append("\n");

            Runtime runtime = Runtime.getRuntime();
            long usedMemory = (runtime.totalMemory() - runtime.freeMemory()) / (1024 * 1024);
            long maxMemory = runtime.maxMemory() / (1024 * 1024);
            sb.append("MemoryInfo Usage: ").append(usedMemory).append("MB / ").append(maxMemory).append("MB\n");

        } catch (Exception e) {
            LOGGER.error("Error collecting device info:" + e.getMessage());
        }
        return sb.toString();
    }
}