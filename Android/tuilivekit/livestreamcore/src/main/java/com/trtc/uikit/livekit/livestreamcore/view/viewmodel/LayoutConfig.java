package com.trtc.uikit.livekit.livestreamcore.view.viewmodel;

import android.text.TextUtils;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import java.lang.reflect.Type;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class LayoutConfig {
    public Map<Integer, LayoutInfo> layoutInfoMap     = new HashMap<>();
    public Set<Integer>             layoutSet         = new HashSet<>();
    public LayoutInfo               defaultLayoutInfo;
    public int                      maxChildViewCount = 9;

    public static LayoutConfig parseJson(String json) {
        if (TextUtils.isEmpty(json)) {
            return null;
        }

        LayoutConfig config = new LayoutConfig();

        try {
            Type type = new TypeToken<Map<Integer, LayoutInfo>>() {
            }.getType();
            config.layoutInfoMap = new Gson().fromJson(json, type);
            if (config.layoutInfoMap == null || config.layoutInfoMap.isEmpty()) {
                return null;
            }
            config.layoutSet = config.layoutInfoMap.keySet();
            if (config.layoutSet.isEmpty()) {
                return null;
            }
            config.maxChildViewCount = Collections.max(config.layoutSet);
            config.defaultLayoutInfo = config.layoutInfoMap.get(config.layoutSet.iterator().next());
        } catch (Exception e) {
            System.out.println("parse LayoutConfig Json error");
        }

        return config;
    }

    public LayoutInfo getLayoutInfo(int viewCount) {
        if (viewCount > maxChildViewCount) {
            return null;
        }
        if (layoutSet.contains(viewCount)) {
            return layoutInfoMap.get(viewCount);
        } else {
            return defaultLayoutInfo;
        }
    }
}
