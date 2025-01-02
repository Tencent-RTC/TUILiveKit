package com.trtc.uikit.component.gift.service;

import com.trtc.uikit.component.gift.store.model.GiftUser;

public class LikePresenter implements LikeIMService.OnLikeMessageListener {

    private final LikeIMService mImService;

    public LikePresenter(String roomId) {
        mImService = new LikeIMService(roomId);
        mImService.setListener(this);
    }

    public void destroyPresenter() {
        if (mImService != null) {
            mImService.setListener(null);
            mImService.unInitImListener();
        }
    }


    public void sendGroupLikeMessage(final GiftCallBack.ActionCallBack callback) {
        mImService.sendGroupLikeMessage(callback);
    }

    @Override
    public void onReceiveLikeMessage(GiftUser sender) {

    }
}
