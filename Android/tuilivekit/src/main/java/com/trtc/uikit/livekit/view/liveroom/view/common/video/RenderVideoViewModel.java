package com.trtc.uikit.livekit.view.liveroom.view.common.video;

import com.trtc.uikit.livekit.state.operation.BattleState;
import com.trtc.uikit.livekit.state.operation.ConnectionState;
import com.trtc.uikit.livekit.state.operation.SeatState;

public class RenderVideoViewModel {
    public String userId;
    public String name;
    public String avatarUrl;
    public String roomId;

    public RenderVideoViewModel() {
    }

    public RenderVideoViewModel(BattleState.BattleUser user) {
        this.userId = user.userId;
        this.name = user.userName;
        this.avatarUrl = user.avatarUrl;
        this.roomId = user.roomId;
    }

    public RenderVideoViewModel(ConnectionState.ConnectionUser user) {
        this.userId = user.userId;
        this.name = user.userName;
        this.avatarUrl = user.avatarUrl;
        this.roomId = user.roomId;
    }

    public RenderVideoViewModel(SeatState.SeatInfo user, String roomId) {
        this.userId = user.userId.get();
        this.name = user.name.get();
        this.avatarUrl = user.avatarUrl.get();
        this.roomId = roomId;
    }
}