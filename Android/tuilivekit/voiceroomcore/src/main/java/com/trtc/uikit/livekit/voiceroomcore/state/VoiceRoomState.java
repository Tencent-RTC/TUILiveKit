package com.trtc.uikit.livekit.voiceroomcore.state;

public class VoiceRoomState {
    public RoomState  roomState  = new RoomState();
    public SeatState  seatState  = new SeatState();
    public UserState  userState  = new UserState();
    public MediaState mediaState = new MediaState();
    public ViewState  viewState  = new ViewState();

    public void reset() {
        roomState.reset();
        seatState.reset();
        userState.reset();
        mediaState.reset();
        viewState.reset();
    }
}
