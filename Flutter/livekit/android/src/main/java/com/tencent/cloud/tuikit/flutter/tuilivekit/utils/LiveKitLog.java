package com.tencent.cloud.tuikit.flutter.tuilivekit.tuilivekit.utils;

import android.content.Context;

import com.tencent.trtc.TRTCCloud;

import org.json.JSONException;
import org.json.JSONObject;

public class LiveKitLog {
    private static final String API                         = "TuikitLog";
    private static final String LOG_KEY_API                 = "api";
    private static final String LOG_KEY_PARAMS              = "params";
    private static final String LOG_KEY_PARAMS_LEVEL        = "level";
    private static final String LOG_KEY_PARAMS_MESSAGE      = "message";
    private static final String LOG_KEY_PARAMS_FILE         = "file";
    private static final String LOG_KEY_PARAMS_MODULE       = "module";
    private static final String LOG_KEY_PARAMS_LINE         = "line";
    private static final String LOG_KEY_PARAMS_FILE_VALUE   = "LiveKitLog";
    private static final String LOG_KEY_PARAMS_MODULE_VALUE = "TUILiveKit";
    private static final int    LOG_KEY_PARAMS_LINE_VALUE   = 0;
    private static final int    LOG_LEVEL_INFO              = 0;
    private static final int    LOG_LEVEL_WARNING           = 1;
    private static final int    LOG_LEVEL_ERROR             = 2;

    public static void error(Context context, String message) {
        log(context, message, LOG_LEVEL_ERROR, LOG_KEY_PARAMS_MODULE_VALUE, LOG_KEY_PARAMS_FILE_VALUE,
                LOG_KEY_PARAMS_LINE_VALUE);
    }

    public static void warn(Context context, String message) {
        log(context, message, LOG_LEVEL_WARNING, LOG_KEY_PARAMS_MODULE_VALUE, LOG_KEY_PARAMS_FILE_VALUE,
                LOG_KEY_PARAMS_LINE_VALUE);
    }

    public static void info(Context context, String message) {
        log(context, message, LOG_LEVEL_INFO, LOG_KEY_PARAMS_MODULE_VALUE, LOG_KEY_PARAMS_FILE_VALUE,
                LOG_KEY_PARAMS_LINE_VALUE);
    }

    private static void log(Context context, String message, int level, String module, String file, int line) {

        try {
            JSONObject paramsJson = new JSONObject();
            paramsJson.put(LOG_KEY_PARAMS_LEVEL, level);
            paramsJson.put(LOG_KEY_PARAMS_MESSAGE, message);
            paramsJson.put(LOG_KEY_PARAMS_MODULE, module);
            paramsJson.put(LOG_KEY_PARAMS_FILE, file);
            paramsJson.put(LOG_KEY_PARAMS_LINE, line);

            JSONObject loggerJson = new JSONObject();
            loggerJson.put(LOG_KEY_API, API);
            loggerJson.put(LOG_KEY_PARAMS, paramsJson);

            TRTCCloud.sharedInstance(context).callExperimentalAPI(loggerJson.toString());
        } catch (JSONException e) {
            throw new RuntimeException(e);
        }

    }
}
