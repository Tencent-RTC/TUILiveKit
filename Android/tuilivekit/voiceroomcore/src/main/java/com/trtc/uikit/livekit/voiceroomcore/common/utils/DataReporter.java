package com.trtc.uikit.livekit.voiceroomcore.common.utils;

import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;

import org.json.JSONException;
import org.json.JSONObject;

public class DataReporter {

    public static final class MetricsEvent {
        public static final int LIVEKIT_METRICS_PANEL_SHOW_SEAT_GRID_VIEW                        = 191026;
        public static final int LIVEKIT_METRICS_PANEL_HIDE_SEAT_GRID_VIEW                        = 191027;
        public static final int LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_START_MICROPHONE      = 191028;
        public static final int LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_STOP_MICROPHONE       = 191029;
        public static final int LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_MUTE_MICROPHONE       = 191030;
        public static final int LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_UNMUTE_MICROPHONE     = 191031;
        public static final int LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_START_ROOM            = 191032;
        public static final int LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_STOP_ROOM             = 191033;
        public static final int LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_JOIN_ROOM             = 191034;
        public static final int LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_LEAVE_ROOM            = 191035;
        public static final int LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_UPDATE_SEAT_MODE      = 191036;
        public static final int LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_RESPONSE_REQUEST      = 191037;
        public static final int LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_CANCEL_REQUEST        = 191038;
        public static final int LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_TAKE_SEAT             = 191039;
        public static final int LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_MOVE_TO_SEAT          = 191040;
        public static final int LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_LEAVE_SEAT            = 191041;
        public static final int LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_TAKE_USER_ON_SEAT     = 191042;
        public static final int LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_KICK_USER_OFF_SEAT    = 191043;
        public static final int LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_LOCK_SEAT             = 191044;
        public static final int LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_SET_LAYOUT_MODE       = 191045;
        public static final int LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_SET_SEAT_VIEW_ADAPTER = 191046;
    }


    public static void reportEventData(int eventKey) {
        try {
            JSONObject params = new JSONObject();
            params.put("key", eventKey);
            JSONObject jsonObject = new JSONObject();
            jsonObject.put("api", "KeyMetricsStats");
            jsonObject.put("params", params);
            String json = jsonObject.toString();
            TUIRoomEngine.callExperimentalAPI(json);
            Logger.info("DataReporter", "reportEventData:[json:" + json + "]");
        } catch (JSONException e) {
            Logger.error("DataReporter", "reportEventData:[e:" + e.getMessage() + "]");
        }
    }
}
