package com.trtc.uikit.component.barrage.view;

import com.tencent.qcloud.tuicore.interfaces.TUICallback;
import com.trtc.uikit.component.barrage.store.model.Barrage;

public interface IBarrageSendView {
    void sendBarrage(Barrage barrage, TUICallback callback);
}
