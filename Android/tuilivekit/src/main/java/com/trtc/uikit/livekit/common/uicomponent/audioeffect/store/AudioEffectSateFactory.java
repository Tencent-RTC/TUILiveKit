package com.trtc.uikit.livekit.common.uicomponent.audioeffect.store;

import android.text.TextUtils;

import java.util.HashMap;
import java.util.Map;

public class AudioEffectSateFactory {
    private static Map<String, AudioEffectState> audioEffectStateMap = new HashMap<>();

    public static AudioEffectState getState(String roomId) {
        if (TextUtils.isEmpty(roomId)) {
            return null;
        }
        AudioEffectState state = audioEffectStateMap.get(roomId);
        if (state == null) {
            state = new AudioEffectState();
            audioEffectStateMap.put(roomId, state);
        }
        return state;
    }

    public static void removeState(String roomId) {
        audioEffectStateMap.remove(roomId);
    }

    public static void removeAllState() {
        audioEffectStateMap.clear();
    }
}
