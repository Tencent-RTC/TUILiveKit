package com.trtc.uikit.livekit.common.uicomponent.barrage.service;

import com.trtc.uikit.livekit.common.uicomponent.barrage.model.TUIBarrage;
import com.trtc.uikit.livekit.common.uicomponent.barrage.view.IBarrageDisplayView;

public interface IBarragePresenter {

    void initDisplayView(IBarrageDisplayView view);

    void destroyPresenter();

    void sendBarrage(TUIBarrage barrage, IBarrageMessage.BarrageSendCallBack callback);

    void receiveBarrage(TUIBarrage barrage);

}
