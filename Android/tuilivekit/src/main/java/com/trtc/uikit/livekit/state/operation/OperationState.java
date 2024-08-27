package com.trtc.uikit.livekit.state.operation;

public class OperationState {
    public RoomState       roomState       = new RoomState();
    public SeatState       seatState       = new SeatState();
    public UserState       userState       = new UserState();
    public MediaState      mediaState      = new MediaState();
    public BeautyState     beautyState     = new BeautyState();
    public ConnectionState connectionState = new ConnectionState();

    public void reset() {
        roomState.reset();
        seatState.reset();
        userState.reset();
        mediaState.reset();
        beautyState.reset();
        connectionState.reset();
    }
}
