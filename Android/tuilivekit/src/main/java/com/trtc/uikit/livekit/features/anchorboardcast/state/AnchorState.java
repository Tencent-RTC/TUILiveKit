package com.trtc.uikit.livekit.features.anchorboardcast.state;

public class AnchorState {
    public RoomState    roomState    = new RoomState();
    public CoGuestState coGuestState = new CoGuestState();
    public CoHostState  coHostState  = new CoHostState();
    public UserState    userState    = new UserState();
    public MediaState   mediaState   = new MediaState();
    public BattleState  battleState  = new BattleState();
}
