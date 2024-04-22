package com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver;

import com.google.gson.Gson;
import com.tencent.qcloud.tuicore.TUIThemeManager;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.GiftBean;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGift;
import com.trtc.uikit.livekit.common.uicomponent.gift.service.TUIHttpGetRequest;
import com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver.IGiftCloudServer.Callback;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.ThreadPoolExecutor;

public class GiftListQueryImpl extends GiftListQuery implements TUIHttpGetRequest.HttpListener {
    private static final String GIFT_DATA_URL =
            "http://dldir1.qq.com/hudongzhibo/TRTC/TUIKit/Gift/gift_data.json";

    private ThreadPoolExecutor            mExecutor;
    private Callback<List<TUIGift>>       mOnGiftListQueryCallback;

    public GiftListQueryImpl(ThreadPoolExecutor executor) {
        this.mExecutor = executor;
    }

    @Override
    public void queryGiftInfoList(Callback<List<TUIGift>> callback) {
        mOnGiftListQueryCallback = callback;
        TUIHttpGetRequest request = new TUIHttpGetRequest(GIFT_DATA_URL, this);
        mExecutor.execute(request);
    }

    @Override
    public void success(String response) {
        handleResponseMessage(response);
    }

    @Override
    public void onFailed(String message) {
        if (mOnGiftListQueryCallback != null) {
            mOnGiftListQueryCallback.onResult(IGiftCloudServer.Error.OPERATION_FAILED, Collections.EMPTY_LIST);
        }
    }

    private void handleResponseMessage(String response) {
        if (response == null) {
            return;
        }
        Gson gson = new Gson();
        GiftBean giftBean = gson.fromJson(response, GiftBean.class);
        final List<TUIGift> giftDataList = transformGiftInfoList(giftBean);
        if (giftDataList != null) {
            if (mOnGiftListQueryCallback != null) {
                mOnGiftListQueryCallback.onResult(IGiftCloudServer.Error.NO_ERROR, giftDataList);
            }
        }
    }

    private List<TUIGift> transformGiftInfoList(GiftBean giftBean) {
        if (giftBean == null) {
            return null;
        }
        List<GiftBean.GiftListBean> giftBeanList = giftBean.getGiftList();
        if (giftBeanList == null) {
            return null;
        }
        boolean isEnglish = Locale.ENGLISH.getLanguage().equals(TUIThemeManager.getInstance().getCurrentLanguage());
        List<TUIGift> giftInfoList = new ArrayList<>();
        for (GiftBean.GiftListBean bean : giftBeanList) {
            TUIGift gift = new TUIGift();
            gift.giftId = bean.getGiftId();
            gift.giftName = isEnglish ? bean.getGiftNameEn() : bean.getGiftName();
            gift.imageUrl = bean.getImageUrl();
            gift.animationUrl = bean.getAnimationUrl();
            gift.price = bean.getPrice();
            giftInfoList.add(gift);
        }
        return giftInfoList;
    }
}
