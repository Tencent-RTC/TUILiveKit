package com.trtc.uikit.livekit.component.barrage.service;

import com.trtc.uikit.livekit.component.barrage.store.model.Barrage;

public interface IBarrageMessage {

    void sendBarrage(Barrage barrage, BarrageSendCallBack callBack);

    void setDelegate(BarrageMessageDelegate delegate);

    interface BarrageMessageDelegate {
        void onReceivedBarrage(Barrage barrage);
    }

    interface BarrageSendCallBack {
        void onSuccess(Barrage barrage);

        void onFailed(int code, String msg);
    }
}
