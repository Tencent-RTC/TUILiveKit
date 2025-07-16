package com.trtc.uikit.livekit.features.audiencecontainer.state;

public class AudienceState {
    public RoomState      roomState      = new RoomState();
    public CoGuestState   coGuestState   = new CoGuestState();
    public UserState      userState      = new UserState();
    public MediaState     mediaState     = new MediaState();
    public BattleState    battleState    = new BattleState();

    public void reset() {
        userState.reset();
        mediaState.reset();
        battleState.reset();
    }
}
