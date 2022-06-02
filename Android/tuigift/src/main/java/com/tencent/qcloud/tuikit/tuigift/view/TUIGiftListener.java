package com.tencent.qcloud.tuikit.tuigift.view;

import com.tencent.qcloud.tuikit.tuigift.model.TUIGiftModel;

/**
 * 礼物发送结果监听
 */
public interface TUIGiftListener {
    void onSendGiftSuccess(int code, String msg, TUIGiftModel giftModel);

    void onSendLikeSuccess(int code, String msg);

    void onFailed(int code, String msg);
}