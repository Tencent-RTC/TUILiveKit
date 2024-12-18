package com.trtc.uikit.livekit.livestreamcore.view.cdnmodel;

import com.google.gson.Gson;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

public class VideoLayoutConfig {

    private static final int DEFAULT_CANVAS_WIDTH  = 720;
    private static final int DEFAULT_CANVAS_HEIGHT = 1280;

    public String layoutJson;

    public VideoCanvasInfo canvas;

    public final List<VideoViewInfo> viewInfoList = new ArrayList<>();

    public static VideoLayoutConfig parseJson(String json) {
        VideoLayoutConfig config = new VideoLayoutConfig();
        try {
            VideoLayoutInfo videoLayoutInfo = new Gson().fromJson(json, VideoLayoutInfo.class);
            if (videoLayoutInfo.layoutList != null && !videoLayoutInfo.layoutList.isEmpty()) {
                config.viewInfoList.addAll(videoLayoutInfo.layoutList);
                JSONArray layoutArray = getJsonArray(videoLayoutInfo.layoutList);
                config.canvas = videoLayoutInfo.canvas == null ? new VideoCanvasInfo() : videoLayoutInfo.canvas;
                if (config.canvas.width == 0 || config.canvas.height == 0) {
                    config.canvas.width = DEFAULT_CANVAS_WIDTH;
                    config.canvas.height = DEFAULT_CANVAS_HEIGHT;
                }
                JSONObject layoutObject = new JSONObject();
                JSONObject styleObject = new JSONObject();
                styleObject.put("viewInfoList", layoutArray);
                styleObject.put("backgroundColor", 0);
                layoutObject.put(videoLayoutInfo.layoutList.size() + "", styleObject);
                config.layoutJson = layoutObject.toString();
            }

        } catch (Exception e) {
            System.out.println("parse ViewLayoutConfig Json error");
        }
        return config;
    }

    private static JSONArray getJsonArray(List<VideoViewInfo> viewInfoList) throws JSONException {
        JSONArray layoutArray = new JSONArray();
        for (int i = 0; i < viewInfoList.size(); i++) {
            VideoViewInfo viewInfoEx = viewInfoList.get(i);
            JSONObject layout = new JSONObject();
            layout.put("x", viewInfoEx.x);
            layout.put("y", viewInfoEx.y);
            layout.put("width", viewInfoEx.width);
            layout.put("height", viewInfoEx.height);
            layout.put("zOrder", viewInfoEx.zOrder);
            layout.put("backgroundColor", 0);
            layoutArray.put(layout);
        }
        return layoutArray;
    }
}
