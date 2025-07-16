package com.trtc.uikit.livekit.features.audiencecontainer.manager.observer;
import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.extension.TUILiveLayoutManager;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.AudienceManager;
import org.json.JSONObject;

import java.lang.ref.WeakReference;

public class LiveLayoutManagerObserver extends TUILiveLayoutManager.Observer {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("LiveLayoutManagerObserver");

    protected WeakReference<AudienceManager> mLiveManager;

    public LiveLayoutManagerObserver(AudienceManager manager) {
        mLiveManager = new WeakReference<>(manager);
    }

    @Override
    public void onLiveVideoLayoutChanged(String roomId, String layoutInfo) {
        LOGGER.info("onLiveVideoLayoutChanged OID:" + hashCode() + " roomId:" + roomId + " layoutInfo:" + layoutInfo);
        if (TextUtils.isEmpty(layoutInfo)) {
            return;
        }
        CanvasInfo canvasInfo = parseCanvasInfo(layoutInfo);
        if (canvasInfo == null) {
            return;
        }
        AudienceManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getRoomManager().onLiveVideoLayoutChanged(roomId, canvasInfo.width, canvasInfo.height);
        }
    }

    private class CanvasInfo {
        private int width;
        private int height;
        public CanvasInfo(int width, int height) {
            this.width = width;
            this.height = height;
        }
    }

    private CanvasInfo parseCanvasInfo(String jsonStr) {
        try {
            JSONObject jsonObject = new JSONObject(jsonStr);
            if (!jsonObject.has("canvas")) {
                return null;
            }
            JSONObject canvas = jsonObject.getJSONObject("canvas");
            if (canvas.has("h") && canvas.has("w")) {
                return new CanvasInfo(canvas.getInt("w"), canvas.getInt("h"));
            }
            return null;
        } catch (Exception e) {
            LOGGER.error("onLiveVideoLayoutChanged parseCanvasInfo fail, parseJson Exception:" + e);
            return null;
        }
    }
}

