package com.tencent.qcloud.tuikit.tuigift.view;

import com.tencent.qcloud.tuikit.tuigift.model.TUIGiftModel;

public interface ITUIGiftPlayView {
    /**
     * 接收礼物
     * @param giftModel 收到的礼物
     */
    void receiveGift(TUIGiftModel giftModel);
}
