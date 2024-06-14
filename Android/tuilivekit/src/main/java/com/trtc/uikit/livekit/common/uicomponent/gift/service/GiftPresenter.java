package com.trtc.uikit.livekit.common.uicomponent.gift.service;

import androidx.core.util.Pair;

import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.uikit.livekit.common.uicomponent.gift.store.GiftSendData;
import com.trtc.uikit.livekit.common.uicomponent.gift.store.GiftStore;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGift;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGiftUser;
import com.trtc.uikit.livekit.common.uicomponent.gift.TUIGiftPlayView;

public class GiftPresenter implements GiftIMService.OnGiftMessageListener, LikeIMService.OnLikeMessageListener {

    private TUIGiftPlayView         mPlayView;
    private final GiftIMService     mImService;
    private final LikeIMService     mLikeImService;
    private final String            mRoomId;


    public GiftPresenter(String roomId) {
        mLikeImService = new LikeIMService(roomId);
        mLikeImService.setListener(this);
        mImService = new GiftIMService(roomId);
        mImService.setListener(this);
        mRoomId = roomId;
    }

    public void initGiftPlayView(TUIGiftPlayView playView) {
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

    public void sendGroupGiftMessage(final TUIGift gift, TUIGiftUser receiver, int giftCount) {
        mImService.sendGroupGiftMessage(gift, receiver, giftCount, (code, msg) -> {
            if (code == 0) {
                final TUIGiftUser sender = new TUIGiftUser();
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
    public void onReceiveLikeMessage(TUIGiftUser sender) {
        if (mPlayView != null) {
            mPlayView.receiveLike();
        }
    }

    @Override
    public void onReceiveGiftMessage(TUIGift gift, int giftCount, TUIGiftUser sender, TUIGiftUser receiver) {
        if (mPlayView != null) {
            mPlayView.receiveGift(gift, giftCount, sender, receiver);
        }
    }
}
