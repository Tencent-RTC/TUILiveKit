package com.trtc.uikit.livekit.liveroom.core;

public class TUILiveDefine {

    public enum RoleType {
        ANCHOR,
        AUDIENCE
    }

    public enum UserInteractionStatus {
        NONE,
        APPLYING,
        LINKING,
        PKING
    }

    public enum InteractionType {
        BROADCAST,
        LINK,
        PK
    }

    public enum UserLiveStatus {
        NONE,
        PREVIEWING,
        PUSHING,
        PLAYING,
        DASHBOARD
    }
}
