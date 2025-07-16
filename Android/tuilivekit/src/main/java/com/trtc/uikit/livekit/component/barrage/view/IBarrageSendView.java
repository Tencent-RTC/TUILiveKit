package com.trtc.uikit.livekit.component.barrage.view;

import com.tencent.qcloud.tuicore.interfaces.TUICallback;
import com.trtc.uikit.livekit.component.barrage.store.model.Barrage;

public interface IBarrageSendView {
    void sendBarrage(Barrage barrage, TUICallback callback);
}
