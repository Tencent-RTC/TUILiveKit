package com.trtc.uikit.livekit.livestreamcore.state;

import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.trtc.tuikit.common.livedata.LiveData;

import java.util.ArrayList;
import java.util.List;

public class CoHostState {
    public final LiveData<List<ConnectionUser>> connectedUserList         = new LiveData<>(new ArrayList<>());
    public final LiveData<List<ConnectionUser>> sentConnectionRequestList = new LiveData<>(new ArrayList<>());
    public final LiveData<ConnectionUser>       receivedConnectionRequest = new LiveData<>();
    public       boolean                        enableConnection          = true;

    public void reset() {
        connectedUserList.get().clear();
        sentConnectionRequestList.get().clear();
        receivedConnectionRequest.set(null);
    }
}
