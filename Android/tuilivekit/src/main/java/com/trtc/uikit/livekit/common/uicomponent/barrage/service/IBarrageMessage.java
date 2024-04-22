package com.trtc.uikit.livekit.common.uicomponent.barrage.service;

import com.trtc.uikit.livekit.common.uicomponent.barrage.model.TUIBarrage;

public interface IBarrageMessage {

    void sendBarrage(TUIBarrage barrage, BarrageSendCallBack callBack);

    void setDelegate(BarrageMessageDelegate delegate);

    interface BarrageMessageDelegate {
        void onReceivedBarrage(TUIBarrage barrage);
    }

    interface BarrageSendCallBack {
        void onSuccess(TUIBarrage barrage);

        void onFailed(int code, String msg);
    }
}
