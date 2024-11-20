package com.trtc.uikit.livekit.voiceroomcore.state;

import com.trtc.tuikit.common.livedata.LiveData;
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine;

public class ViewState {
    public LiveData<VoiceRoomDefine.LayoutMode>           layoutMode   =
            new LiveData<>(VoiceRoomDefine.LayoutMode.GRID);
    public LiveData<VoiceRoomDefine.SeatViewLayoutConfig> layoutConfig =
            new LiveData<>(new VoiceRoomDefine.SeatViewLayoutConfig());

    public void reset() {
    }
}
