package com.trtc.uikit.livekit.common.uicomponent.gift.service;

import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGiftUser;

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
    public void onReceiveLikeMessage(TUIGiftUser sender) {

    }
}
