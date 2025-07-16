package com.trtc.uikit.livekit.component.gift.service;

import static com.trtc.uikit.livekit.component.gift.service.GiftConstants.LANGUAGE_EN;
import static com.trtc.uikit.livekit.component.gift.service.GiftConstants.LANGUAGE_ZH_HANS;
import static com.trtc.uikit.livekit.component.gift.service.GiftConstants.LANGUAGE_ZH_HANT;

import android.text.TextUtils;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveGiftManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveGiftManager.GiftInfo;
import com.tencent.cloud.tuikit.engine.extension.TUILiveGiftManager.Observer;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.gift.store.TUIGiftStore;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Responsible for handling the messaging service for sending and receiving gifts
 */
public class GiftService {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getComponentLogger("GiftService");

    private String              mRoomId;
    private TUILiveGiftManager  mGiftManager;
    private GiftServiceDelegate mDelegate;

    public GiftService() {

    }

    public void init(String roomId, GiftServiceDelegate delegate) {
        LOGGER.info("init, roomId:" + roomId + ", delegate:" + delegate);
        mRoomId = roomId;
        mGiftManager = (TUILiveGiftManager) TUIRoomEngine.sharedInstance().getExtension(TUICommonDefine.ExtensionType.LIVE_GIFT_MANAGER);
        if (delegate != null) {
            mGiftManager.addObserver(mGiftObserver);
        }
        mDelegate = delegate;
        String language = getCurrentLanguage();
        TUIGiftStore.sharedInstance().mSystemLanguage.setValue(language);
        setCurrentLanguage(language);
    }

    public void unInit() {
        LOGGER.info("unInit, mRoomId:" + mRoomId);
        if (mGiftManager != null) {
            mGiftManager.removeObserver(mGiftObserver);
        }
    }

    public String getRoomId() {
        return mRoomId;
    }

    private String getCurrentLanguage() {
        String language = Locale.getDefault().getLanguage();
        String languageTag = Locale.getDefault().toLanguageTag();
        LOGGER.info("language:" + language + ", languageTag:" + languageTag);
        if (TextUtils.isEmpty(language) || TextUtils.isEmpty(languageTag)) {
            return LANGUAGE_EN;
        }
        languageTag = languageTag.toLowerCase();
        if ("zh".equalsIgnoreCase(language)) {
            if (languageTag.contains("zh-hans")
                    || languageTag.equals("zh")
                    || languageTag.equals("zh-cn")
                    || languageTag.equals("zh-sg")
                    || languageTag.equals("zh-my")) {
                return LANGUAGE_ZH_HANS;
            } else {
                return LANGUAGE_ZH_HANT;
            }
        } else {
            return LANGUAGE_EN;
        }
    }

    private void setCurrentLanguage(String language) {
        LOGGER.info("setCurrentLanguage:" + language);
        try {
            JSONObject params = new JSONObject();
            params.put("language", language);
            JSONObject jsonObject = new JSONObject();
            jsonObject.put("api", "setCurrentLanguage");
            jsonObject.put("params", params);
            String json = jsonObject.toString();
            TUIRoomEngine.sharedInstance().callExperimentalAPI(json, null);
        } catch (JSONException e) {
            LOGGER.error("setCurrentLanguage failed:" + e.getLocalizedMessage());
        }
    }

    public void updateGiftList() {
        TUIGiftStore giftStore = TUIGiftStore.sharedInstance();
        String language = getCurrentLanguage();
        LOGGER.info("current system language:" + language);
        Map<String, List<GiftInfo>> giftListMap = giftStore.mGiftListMap.getValue();
        boolean isGiftListEmpty = giftListMap == null || giftListMap.isEmpty();
        boolean isLanguageChanged = !TextUtils.equals(giftStore.mSystemLanguage.getValue(), language);
        if (isLanguageChanged) {
            giftStore.mSystemLanguage.setValue(language);
            setCurrentLanguage(language);
        }
        if (isLanguageChanged || isGiftListEmpty) {
            getGiftList();
        }
    }

    private void getGiftList() {
        LOGGER.info("getGiftList, mRoomId:" + mRoomId);
        if (mGiftManager == null || TextUtils.isEmpty(mRoomId)) {
            LOGGER.error("sendGift failed, mGiftManager or mRoomId is null");
            return;
        }
        mGiftManager.getGiftList(mRoomId, new TUILiveGiftManager.GetGiftListCallback() {
            @Override
            public void onSuccess(List<TUILiveGiftManager.GiftCategory> list) {
                List<GiftInfo> giftInfoList = new ArrayList<>();
                for (TUILiveGiftManager.GiftCategory category : list) {
                    giftInfoList.addAll(category.giftList);
                }
                Map<String, List<GiftInfo>> map = TUIGiftStore.sharedInstance().mGiftListMap.getValue();
                if (map != null) {
                    map.put(mRoomId, giftInfoList);
                    TUIGiftStore.sharedInstance().mGiftListMap.setValue(map);
                }
                LOGGER.info("getGiftList, size:" + giftInfoList.size());
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("getGiftList onError, errorCode:" + error.getValue() + ",message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    public void sendGift(GiftInfo giftInfo, int giftCount) {
        LOGGER.info("sendGift, mRoomId:" + mRoomId + ", giftInfo:" + new Gson().toJson(giftInfo) + ", giftCount:" + giftCount);
        if (mGiftManager == null || giftInfo == null || TextUtils.isEmpty(mRoomId)) {
            LOGGER.error("sendGift failed, mGiftManager or giftInfo or mRoomId is null");
            return;
        }
        mGiftManager.sendGift(mRoomId, giftInfo.giftId, giftCount, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {

            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("sendGift onError, errorCode:" + error.getValue() + ",message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    public void sendLike(int count, TUIRoomDefine.ActionCallback callback) {
        LOGGER.info("sendLike, mRoomId:" + mRoomId + ", count:" + count);
        if (mGiftManager == null || TextUtils.isEmpty(mRoomId) || count <= 0) {
            LOGGER.error("sendLike failed, mGiftManager or mRoomId is null");
            return;
        }
        mGiftManager.sendLike(mRoomId, count, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("sendLike onError, errorCode:" + error.getValue() + ",message:" + message);
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    private final Observer mGiftObserver = new Observer() {
        @Override
        public void onGiftCountChanged(String roomId, long totalGiftsSent, long totalGiftCoins, long totalUniqueGiftSenders) {
            LOGGER.info("onGiftCountChanged roomId:" + roomId + ", totalGiftsSent:" + totalGiftsSent
                    + ", totalGiftCoins:" + totalGiftCoins + ", totalUniqueGiftSenders:" + totalUniqueGiftSenders);
        }

        @Override
        public void onReceiveGiftMessage(String roomId, GiftInfo giftInfo, long giftCount, UserInfo sender) {
            LOGGER.info("onReceiveGiftMessage roomId:" + roomId + ", giftInfo:" + ((giftInfo == null) ? "null" : giftInfo.giftId)
                    + ", giftCount:" + giftCount + ", sender:" + ((sender == null) ? "null" : sender.userId));
            if (TextUtils.equals(mRoomId, roomId) && mDelegate != null) {
                mDelegate.onReceiveGiftMessage(giftInfo, (int) giftCount, sender);
            }
        }

        @Override
        public void onReceiveLikesMessage(String roomId, long totalLikesReceived, UserInfo sender) {
            LOGGER.info("onReceiveLikesMessage roomId:" + roomId + ", totalLikesReceived:" + totalLikesReceived
                    + ", sender:" + ((sender == null) ? "null" : sender.userId));
            if (!TextUtils.equals(mRoomId, roomId)) {
                return;
            }
            Long localLikeCount = TUIGiftStore.sharedInstance().mLocalLikeCountMap.get(roomId);
            localLikeCount = localLikeCount == null ? 0 : localLikeCount;
            int diffLikes = (int) (totalLikesReceived - localLikeCount);
            if (diffLikes > 0) {
                localLikeCount = totalLikesReceived;
                TUIGiftStore.sharedInstance().mLocalLikeCountMap.put(roomId, localLikeCount);
                LOGGER.info("diffLikes:" + diffLikes);
                if (mDelegate != null) {
                    mDelegate.onReceiveLikesMessage(diffLikes, sender);
                }
            }
        }
    };

    public interface GiftServiceDelegate {
        void onReceiveGiftMessage(GiftInfo giftInfo, int giftCount, UserInfo sender);

        void onReceiveLikesMessage(int totalLikesDiff, TUIRoomDefine.UserInfo sender);
    }
}
