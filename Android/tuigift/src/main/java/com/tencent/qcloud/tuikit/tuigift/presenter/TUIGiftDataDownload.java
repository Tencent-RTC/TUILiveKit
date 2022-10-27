package com.tencent.qcloud.tuikit.tuigift.presenter;

import com.tencent.qcloud.tuikit.tuigift.model.TUIGiftModel;

import java.util.List;

/**
 * 获取后台礼物信息
 */
public class TUIGiftDataDownload {
    private static final String TAG = "TUIGiftDataDownload";

    private TUIGiftListQuery mTUIGiftListQuery;

    public void setGiftListQuery(TUIGiftListQuery tuiGiftListQuery) {
        mTUIGiftListQuery = tuiGiftListQuery;
        queryGiftInfoList(null);
    }

    public void queryGiftInfoList(final GiftQueryCallback callback) {
        if (mTUIGiftListQuery != null) {
            mTUIGiftListQuery.queryGiftInfoList(new TUIGiftCallBack.OnGiftListQueryCallback() {
                @Override
                public void onGiftListQuerySuccess(List<TUIGiftModel> giftDataList) {
                    if (callback != null) {
                        callback.onQuerySuccess(giftDataList);
                    }
                }

                @Override
                public void onGiftListQueryFailed(String errorMessage) {
                    if (callback != null) {
                        callback.onQueryFailed(errorMessage);
                    }
                }
            });
        }
    }

    public interface GiftQueryCallback {
        void onQuerySuccess(List<TUIGiftModel> giftInfoList);

        void onQueryFailed(String errorMsg);
    }

}
