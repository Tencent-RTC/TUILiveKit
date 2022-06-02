package com.tencent.qcloud.tuikit.tuigift.view;

import com.tencent.qcloud.tuikit.tuigift.model.TUIGiftModel;

public interface ITUIGiftListPanelView {
    /**
     * 发送礼物
     *
     * @param giftModel 待发送礼物
     */
    void sendGift(TUIGiftModel giftModel);

    /**
     * 点赞
     */
    void sendLike();
}
