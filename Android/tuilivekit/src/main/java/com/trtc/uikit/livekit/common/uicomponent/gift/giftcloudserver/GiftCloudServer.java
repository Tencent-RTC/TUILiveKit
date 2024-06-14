package com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver;

import static com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver.IGiftCloudServer.Error.BALANCE_INSUFFICIENT;
import static com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver.IGiftCloudServer.Error.NO_ERROR;
import static com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver.IGiftCloudServer.Error.PARAM_ERROR;

import android.annotation.TargetApi;
import android.os.Build;

import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGift;

import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class GiftCloudServer implements IGiftCloudServer {

    private static final int CORE_POOL_SIZE = 5;

    private GiftListQuery      mGiftListQuery;
    private ThreadPoolExecutor mExecutor;
    private int                mBalance = 500;

    public GiftCloudServer() {
        mExecutor = getThreadExecutor();
        mGiftListQuery = new GiftListQueryImpl(mExecutor);
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
        mGiftListQuery.queryGiftInfoList(callback);
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

    private synchronized ThreadPoolExecutor getThreadExecutor() {
        if (mExecutor == null || mExecutor.isShutdown()) {
            mExecutor = new GiftThreadPool(CORE_POOL_SIZE);
        }
        return mExecutor;
    }

    private static class GiftThreadPool extends ThreadPoolExecutor {
        @TargetApi(Build.VERSION_CODES.GINGERBREAD)
        public GiftThreadPool(int poolSize) {
            super(poolSize, poolSize, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingDeque<Runnable>(),
                    Executors.defaultThreadFactory(), new AbortPolicy());
        }
    }

}
