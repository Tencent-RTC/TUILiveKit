package com.trtc.uikit.livekit.livestreamcore.state;

import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleUser;
import com.trtc.tuikit.common.livedata.LiveData;

import java.util.ArrayList;
import java.util.List;

public class BattleState {

    public String battleId;

    public final LiveData<List<String>>     sentBattleRequestList = new LiveData<>(new ArrayList<>());
    public final LiveData<List<BattleUser>> mBattledUsers         = new LiveData<>(new ArrayList<>());

    public void reset() {
        battleId = "";
        sentBattleRequestList.get().clear();
        mBattledUsers.get().clear();
    }
}
