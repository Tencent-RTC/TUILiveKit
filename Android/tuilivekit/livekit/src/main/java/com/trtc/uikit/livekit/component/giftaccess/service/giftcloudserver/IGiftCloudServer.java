package com.trtc.uikit.livekit.component.giftaccess.service.giftcloudserver;

import com.trtc.uikit.livekit.component.gift.store.model.Gift;

import java.util.List;

public interface IGiftCloudServer {

    void rechargeBalance(Callback<Integer> callback);

    void queryBalance(Callback<Integer> callback);

    void queryGiftInfoList(Callback<List<Gift>> callback);

    void sendGift(String sender, String receiver, Gift gift, int giftCount, Callback<Integer> callback);

    interface Callback<T> {
        void onResult(int error, T result);
    }

    interface Error {
        int NO_ERROR = 0;
        int OPERATION_FAILED = -1;
        int PARAM_ERROR = -2;
        int BALANCE_INSUFFICIENT = -3;
    }
}
