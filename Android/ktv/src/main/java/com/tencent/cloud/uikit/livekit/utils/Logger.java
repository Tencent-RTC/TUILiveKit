package com.tencent.cloud.uikit.livekit.utils;

import android.util.Log;

import com.tencent.trtc.TRTCCloud;
import com.trtc.tuikit.common.system.ContextProvider;

import org.json.JSONException;
import org.json.JSONObject;

public final class Logger {

    public static final String MODULE_KTV_ROOM = "KTVRoom";

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

    public Logger(String module, String file) {
        mModuleName = module;
        mFileName = file;
    }

    public static Logger getKTVLogger(String file) {
        return new Logger(MODULE_KTV_ROOM, file);
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
