package com.trtc.uikit.livekit.component.barrage.service;

import com.trtc.uikit.livekit.component.barrage.store.model.Barrage;
import com.trtc.uikit.livekit.component.barrage.view.IBarrageDisplayView;

public interface IBarragePresenter {

    void initDisplayView(IBarrageDisplayView view);

    void destroyPresenter();

    void sendBarrage(Barrage barrage, IBarrageMessage.BarrageSendCallBack callback);

    void receiveBarrage(Barrage barrage);

}
