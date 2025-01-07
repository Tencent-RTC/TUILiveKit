package com.trtc.uikit.livekit.voiceroomcore.state;

public class MediaState {
    public boolean hasMicrophonePermission;
    public boolean isMicrophoneOpened;

    public void reset() {
        isMicrophoneOpened = false;
    }
}
