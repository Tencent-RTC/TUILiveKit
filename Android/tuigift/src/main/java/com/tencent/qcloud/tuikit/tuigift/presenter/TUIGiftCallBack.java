package com.tencent.qcloud.tuikit.tuigift.presenter;

import com.tencent.qcloud.tuikit.tuigift.model.TUIGiftModel;

import java.util.List;

/**
 * 礼物回调封装
 */
public class TUIGiftCallBack {
    /**
     * 通用回调
     */
    public interface ActionCallBack {
        void onCallback(int code, String msg);
    }


    /**
     * 礼物发送结果回调
     */
    public interface GiftSendCallBack {
        void onSuccess(int code, String msg, TUIGiftModel giftModel);

        void onFailed(int code, String msg);
    }


    /**
     * 礼物信息查询回调
     */
    public interface OnGiftListQueryCallback {
        /**
         * 查询成功 响应结果
         *
         * @param giftInfoList 查询到的礼物List
         */
        void onGiftListQuerySuccess(List<TUIGiftModel> giftInfoList);

        /**
         * 查询失败
         */
        void onGiftListQueryFailed(String errorMsg);
    }
}
