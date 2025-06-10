package com.trtc.uikit.livekit.common;

import android.util.Log;

import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;

import org.json.JSONException;
import org.json.JSONObject;

public class DataReporter {
    public static void reportEventData(int eventKey) {
        try {
            JSONObject params = new JSONObject();
            params.put("key", eventKey);
            JSONObject jsonObject = new JSONObject();
            jsonObject.put("api", "KeyMetricsStats");
            jsonObject.put("params", params);
            String json = jsonObject.toString();
            TUIRoomEngine.sharedInstance().callExperimentalAPI(json, null);
            Log.i("DataReporter", "reportEventData:[json:" + json + "]");
        } catch (JSONException e) {
            Log.e("DataReporter", "reportEventData:[e:" + e.getMessage() + "]");
        }
    }
}
