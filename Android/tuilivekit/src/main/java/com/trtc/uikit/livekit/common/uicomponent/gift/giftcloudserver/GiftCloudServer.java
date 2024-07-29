package com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver;

import static com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver.GiftCloudServerConfig.GIFT_DATA_URL;
import static com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver.GiftCloudServerConfig.TE_GIFT_DATA_URL;
import static com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver.IGiftCloudServer.Error.BALANCE_INSUFFICIENT;
import static com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver.IGiftCloudServer.Error.NO_ERROR;
import static com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver.IGiftCloudServer.Error.PARAM_ERROR;

import com.google.gson.Gson;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.TUIThemeManager;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.GiftBean;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGift;
import com.trtc.uikit.livekit.common.utils.HttpGetRequest;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

public class GiftCloudServer implements IGiftCloudServer {

    private final String mGiftUrl;

    private int mBalance = 500;

    private final List<TUIGift> mCacheGiftList = new ArrayList<>();

    public GiftCloudServer() {
        mGiftUrl = TUICore.getService("TUIEffectPlayerService") == null ? GIFT_DATA_URL : TE_GIFT_DATA_URL;
    }

    @Override
    public void rechargeBalance(Callback<Integer> callback) {
        mBalance += 100;
        if (callback != null) {
            callback.onResult(NO_ERROR, mBalance);
        }
    }

    @Override
    public void queryBalance(Callback<Integer> callback) {
        if (callback != null) {
            callback.onResult(NO_ERROR, mBalance);
        }
    }

    @Override
    public void queryGiftInfoList(Callback<List<TUIGift>> callback) {
        if (mCacheGiftList.isEmpty()) {
            queryGiftList((error, result) -> {
                if (error == 0) {
                    synchronized (GiftCloudServer.this) {
                        if (!mCacheGiftList.isEmpty()) {
                            mCacheGiftList.clear();
                        }
                        mCacheGiftList.addAll(result);
                    }
                }
                if (callback != null) {
                    callback.onResult(error, result);
                }
            });
        } else {
            if (callback != null) {
                callback.onResult(NO_ERROR, mCacheGiftList);
            }
        }
    }

    @Override
    public void sendGift(String sender, String receiver, TUIGift gift, int giftCount,
                         Callback<Integer> callback) {
        if (gift == null || giftCount <= 0) {
            if (callback != null) {
                callback.onResult(PARAM_ERROR, mBalance);
            }
            return;
        }
        int newBalance = mBalance - giftCount * gift.price;
        if (newBalance >= 0) {
            mBalance = newBalance;
            if (callback != null) {
                callback.onResult(NO_ERROR, newBalance);
            }
        } else {
            if (callback != null) {
                callback.onResult(BALANCE_INSUFFICIENT, mBalance);
            }
        }
    }

    private void queryGiftList(Callback<List<TUIGift>> callback) {
        HttpGetRequest request = new HttpGetRequest(mGiftUrl, new HttpGetRequest.HttpListener() {
            @Override
            public void onSuccess(String response) {
                Gson gson = new Gson();
                GiftBean giftBean = gson.fromJson(response, GiftBean.class);
                final List<TUIGift> giftDataList = transformGiftInfoList(giftBean);
                if (giftDataList != null) {
                    if (callback != null) {
                        callback.onResult(NO_ERROR, giftDataList);
                    }
                } else {
                    onFailed("query gift list failed!");
                }
            }

            @Override
            public void onFailed(String message) {
                if (callback != null) {
                    callback.onResult(IGiftCloudServer.Error.OPERATION_FAILED, Collections.EMPTY_LIST);
                }
            }
        });
        request.execute();
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
