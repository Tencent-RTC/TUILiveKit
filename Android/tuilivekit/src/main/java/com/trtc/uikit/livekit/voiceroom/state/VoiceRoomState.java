package com.trtc.uikit.livekit.voiceroom.state;

public class VoiceRoomState {
    public RoomState  roomState  = new RoomState();
    public SeatState  seatState  = new SeatState();
    public UserState  userState  = new UserState();
    public MediaState mediaState = new MediaState();

    public void reset() {
        roomState.reset();
        seatState.reset();
        userState.reset();
        mediaState.reset();
    }
}
