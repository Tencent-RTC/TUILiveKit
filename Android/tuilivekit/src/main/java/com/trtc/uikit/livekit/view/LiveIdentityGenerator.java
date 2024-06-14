package com.trtc.uikit.livekit.view;

import androidx.annotation.NonNull;

public final class LiveIdentityGenerator {
    private static final LiveIdentityGenerator INSTANCE = new LiveIdentityGenerator();

    public static LiveIdentityGenerator getInstance() {
        return INSTANCE;
    }

    private LiveIdentityGenerator() {
    }

    public String generateId(String id, @NonNull RoomType type) {
        return type.getPrefix() + id;
    }

    public RoomType getIDType(String id) {
        if (id == null) {
            return null;
        }
        for (RoomType type : RoomType.values()) {
            if (id.startsWith(type.getPrefix())) {
                return type;
            }
        }
        return null;
    }

    public enum RoomType {
        LIVE("live_"),
        VOICE("voice_");

        private final String prefix;

        RoomType(String prefix) {
            this.prefix = prefix;
        }

        public String getPrefix() {
            return prefix;
        }
    }
}
