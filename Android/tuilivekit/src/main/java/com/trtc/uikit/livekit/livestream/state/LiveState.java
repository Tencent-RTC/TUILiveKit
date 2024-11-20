package com.trtc.uikit.livekit.livestream.state;

public class LiveState {
    public RoomState      roomState      = new RoomState();
    public CoGuestState   coGuestState   = new CoGuestState();
    public CoHostState    coHostState    = new CoHostState();
    public UserState      userState      = new UserState();
    public MediaState     mediaState     = new MediaState();
    public BeautyState    beautyState    = new BeautyState();
    public BattleState    battleState    = new BattleState();
    public DashboardState dashboardState = new DashboardState();

    public void reset() {
        roomState.reset();
        coGuestState.reset();
        userState.reset();
        mediaState.reset();
        beautyState.reset();
        coHostState.reset();
        battleState.reset();
    }
}
