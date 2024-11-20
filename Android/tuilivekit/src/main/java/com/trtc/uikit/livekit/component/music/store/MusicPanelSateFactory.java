package com.trtc.uikit.livekit.component.music.store;

import android.text.TextUtils;

import java.util.HashMap;
import java.util.Map;

public class MusicPanelSateFactory {
    private static Map<String, MusicPanelState> musicPanelStateMap = new HashMap<>();

    public static MusicPanelState getState(String roomId) {
        if (TextUtils.isEmpty(roomId)) {
            return null;
        }
        MusicPanelState state = musicPanelStateMap.get(roomId);
        if (state == null) {
            state = new MusicPanelState();
            musicPanelStateMap.put(roomId, state);
        }
        return state;
    }

    public static void removeState(String roomId) {
        musicPanelStateMap.remove(roomId);
    }

    public static void removeAllState() {
        musicPanelStateMap.clear();
    }
}
