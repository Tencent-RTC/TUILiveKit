package com.trtc.uikit.livekit.example.view.main.model;

public enum MainTypeEnum {
    TYPE_VIDEO_LIVE(100, "video_live"),
    TYPE_VOICE_ROOM(101, "voice_room");

    private final int    mType;
    private final String mProperties;

    MainTypeEnum(int mType, String mProperties) {
        this.mType = mType;
        this.mProperties = mProperties;
    }

    public int getType() {
        return mType;
    }

    public String getProperties() {
        return mProperties;
    }
}
