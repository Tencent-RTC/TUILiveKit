package com.trtc.uikit.component.gift.service;

import androidx.core.util.Pair;

import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.uikit.component.gift.GiftPlayView;
import com.trtc.uikit.component.gift.store.GiftSendData;
import com.trtc.uikit.component.gift.store.GiftStore;
import com.trtc.uikit.component.gift.store.model.Gift;
import com.trtc.uikit.component.gift.store.model.GiftUser;

public class GiftPresenter implements GiftIMService.OnGiftMessageListener, LikeIMService.OnLikeMessageListener {

    private       GiftPlayView  mPlayView;
    private final GiftIMService mImService;
    private final LikeIMService mLikeImService;
    private final String        mRoomId;


    public GiftPresenter(String roomId) {
        mLikeImService = new LikeIMService(roomId);
        mLikeImService.setListener(this);
        mImService = new GiftIMService(roomId);
        mImService.setListener(this);
        mRoomId = roomId;
    }

    public void initGiftPlayView(GiftPlayView playView) {
        this.mPlayView = playView;
    }

    public void destroyPresenter() {
        mPlayView = null;
        if (mImService != null) {
            mImService.setListener(null);
            mImService.unInitImListener();
        }
        if (mLikeImService != null) {
            mLikeImService.setListener(null);
            mLikeImService.unInitImListener();
        }
    }

    public void sendGroupGiftMessage(final Gift gift, GiftUser receiver, int giftCount) {
        mImService.sendGroupGiftMessage(gift, receiver, giftCount, (code, msg) -> {
            if (code == 0) {
                final GiftUser sender = new GiftUser();
                sender.userId = TUILogin.getUserId();
                sender.userName = TUILogin.getNickName();
                sender.avatarUrl = TUILogin.getFaceUrl();
                sender.level = "0";

                GiftSendData sendData = new GiftSendData();
                sendData.sender = sender;
                sendData.receiver = receiver;
                sendData.gift = gift;
                sendData.giftCount = giftCount;
                GiftStore.getInstance().mGiftSendData.set(new Pair<>(mRoomId, sendData));
            }
        });
    }

    @Override
    public void onReceiveLikeMessage(GiftUser sender) {
        if (mPlayView != null) {
            mPlayView.receiveLike();
        }
    }

    @Override
    public void onReceiveGiftMessage(Gift gift, int giftCount, GiftUser sender, GiftUser receiver) {
        if (mPlayView != null) {
            mPlayView.receiveGift(gift, giftCount, sender, receiver);
        }
    }
}
