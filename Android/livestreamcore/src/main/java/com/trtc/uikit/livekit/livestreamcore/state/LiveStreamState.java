package com.trtc.uikit.livekit.livestreamcore.state;

public class LiveStreamState {
    public RoomState    roomState    = new RoomState();
    public UserState    userState    = new UserState();
    public MediaState   mediaState   = new MediaState();
    public CoHostState  coHostState  = new CoHostState();
    public CoGuestState coGuestState = new CoGuestState();

    public void reset() {
        roomState.reset();
        coGuestState.reset();
        userState.reset();
        mediaState.reset();
        coHostState.reset();
    }
}
