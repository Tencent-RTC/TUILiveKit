package com.trtc.uikit.livekit.livestreamcore.common.utils;

import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;

import org.json.JSONException;
import org.json.JSONObject;

public class DataReporter {

    public static final class MetricsEvent {
        public static final int LIVEKIT_METRICS_PANEL_SHOW_LIVE_STREAM_LIST_VIEW                        = 190026;
        public static final int LIVEKIT_METRICS_PANEL_HIDE_LIVE_STREAM_LIST_VIEW                        = 190027;
        public static final int LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_START_CAMERA                    = 190028;
        public static final int LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_START_MICROPHONE                = 190029;
        public static final int LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_MUTE_MICROPHONE                 = 190030;
        public static final int LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_STOP_CAMERA                     = 190031;
        public static final int LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_STOP_MICROPHONE                 = 190032;
        public static final int LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_CREATE_ROOM                     = 190033;
        public static final int LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_DESTROY_ROOM                    = 190034;
        public static final int LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_JOIN_ROOM                       = 190035;
        public static final int LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_LEAVE_ROOM                      = 190036;
        public static final int LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_REQUEST_INTRA_ROOM_CONNECTION   = 190037;
        public static final int LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_CANCEL_INTRA_ROOM_CONNECTION    = 190038;
        public static final int LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_RESPOND_INTRA_ROOM_CONNECTION   = 190039;
        public static final int LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_DISCONNECT_USER                 = 190040;
        public static final int LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_TERMINATE_INTRA_ROOM_CONNECTION = 190041;
        public static final int LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_REQUEST_CROSS_ROOM_CONNECTION   = 190042;
        public static final int LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_CANCEL_CROSS_ROOM_CONNECTION    = 190043;
        public static final int LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_RESPOND_CROSS_ROOM_CONNECTION   = 190044;
        public static final int LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_TERMINATE_CROSS_ROOM_CONNECTION = 190045;
        public static final int LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_SET_LAYOUT_MODE                 = 190046;
        public static final int LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_SET_VIDEO_VIEW_ADAPTER          = 190047;
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
            Logger.info("DataReporter reportEventData:[json:" + json + "]");
        } catch (JSONException e) {
            Logger.error("DataReporter reportEventData:[e:" + e.getMessage() + "]");
        }
    }
}
