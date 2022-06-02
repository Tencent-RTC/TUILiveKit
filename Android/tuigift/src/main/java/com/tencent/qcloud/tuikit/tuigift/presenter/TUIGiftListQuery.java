package com.tencent.qcloud.tuikit.tuigift.presenter;


/**
 * 礼物数据查询
 */
public abstract class TUIGiftListQuery {
    /**
     * 查询礼物信息
     *
     * @param callback 查询结果回调
     */
    public abstract void queryGiftInfoList(TUIGiftCallBack.OnGiftListQueryCallback callback);
}
