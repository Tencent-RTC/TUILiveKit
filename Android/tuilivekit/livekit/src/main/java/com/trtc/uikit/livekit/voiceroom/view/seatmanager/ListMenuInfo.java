package com.trtc.uikit.livekit.voiceroom.view.seatmanager;

public class ListMenuInfo {
    public ListMenuInfo(String text, OnClickListener listener) {
        this.text = text;
        this.listener = listener;
    }

    public String          text;
    public OnClickListener listener;

    public interface OnClickListener {
        void onClick();
    }
}
