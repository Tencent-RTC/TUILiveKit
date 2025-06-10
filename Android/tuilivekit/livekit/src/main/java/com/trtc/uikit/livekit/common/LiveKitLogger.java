package com.trtc.uikit.livekit.common;

import android.util.Log;

import com.tencent.trtc.TRTCCloud;
import com.trtc.tuikit.common.system.ContextProvider;

import org.json.JSONException;
import org.json.JSONObject;

public class LiveKitLogger {
    public static final String MODULE_LIVE_STREAM_CORE = "KitLiveStream";
    public static final String MODULE_SEAT_GRID_CORE   = "KitVoiceRoom";
    public static final String MODULE_FEATURES         = "KitFeatures";
    public static final String MODULE_COMPONENT        = "KitComponent";
    public static final String MODULE_COMMON           = "KitCommon";

    private static final String API                    = "TuikitLog";
    private static final String LOG_KEY_API            = "api";
    private static final String LOG_KEY_PARAMS         = "params";
    private static final String LOG_KEY_PARAMS_LEVEL   = "level";
    private static final String LOG_KEY_PARAMS_MESSAGE = "message";
    private static final String LOG_KEY_PARAMS_FILE    = "file";
    private static final String LOG_KEY_PARAMS_MODULE  = "module";
    private static final String LOG_KEY_PARAMS_LINE    = "line";
    private static final int    LOG_LEVEL_INFO         = 0;
    private static final int    LOG_LEVEL_WARNING      = 1;
    private static final int    LOG_LEVEL_ERROR        = 2;

    private final String mModuleName;
    private final String mFileName;

    public LiveKitLogger(String module, String file) {
        mModuleName = module;
        mFileName = file;
    }

    public static LiveKitLogger getLiveStreamLogger(String file) {
        return new LiveKitLogger(MODULE_LIVE_STREAM_CORE, file);
    }

    public static LiveKitLogger getVoiceRoomLogger(String file) {
        return new LiveKitLogger(MODULE_SEAT_GRID_CORE, file);
    }

    public static LiveKitLogger getFeaturesLogger(String file) {
        return new LiveKitLogger(MODULE_FEATURES, file);
    }

    public static LiveKitLogger getComponentLogger(String file) {
        return new LiveKitLogger(MODULE_COMPONENT, file);
    }

    public static LiveKitLogger getCommonLogger(String file) {
        return new LiveKitLogger(MODULE_COMMON, file);
    }

    public void info(String message) {
        log(LOG_LEVEL_INFO, message);
    }

    public void warn(String message) {
        log(LOG_LEVEL_WARNING, message);
    }

    public void error(String message) {
        log(LOG_LEVEL_ERROR, message);
    }

    private void log(int level, String message) {
        log(mModuleName, mFileName, level, message);
    }

    public static void info(String module, String file, String message) {
        log(module, file, LOG_LEVEL_INFO, message);
    }

    public static void warn(String module, String file, String message) {
        log(module, file, LOG_LEVEL_WARNING, message);
    }

    public static void error(String module, String file, String message) {
        log(module, file, LOG_LEVEL_ERROR, message);
    }

    private static void log(String module, String file, int level, String message) {
        try {
            JSONObject paramsJson = new JSONObject();
            paramsJson.put(LOG_KEY_PARAMS_LEVEL, level);
            paramsJson.put(LOG_KEY_PARAMS_MESSAGE, message);
            paramsJson.put(LOG_KEY_PARAMS_MODULE, module);
            paramsJson.put(LOG_KEY_PARAMS_FILE, file);
            paramsJson.put(LOG_KEY_PARAMS_LINE, 0);

            JSONObject loggerJson = new JSONObject();
            loggerJson.put(LOG_KEY_API, API);
            loggerJson.put(LOG_KEY_PARAMS, paramsJson);

            TRTCCloud.sharedInstance(ContextProvider.getApplicationContext()).callExperimentalAPI(loggerJson.toString());
        } catch (JSONException e) {
            Log.e("Logger", e.toString());
        }
    }

}
