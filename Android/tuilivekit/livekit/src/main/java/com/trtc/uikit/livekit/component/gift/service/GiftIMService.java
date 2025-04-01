package com.trtc.uikit.livekit.component.gift.service;

import android.text.TextUtils;
import android.util.Log;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.tencent.imsdk.v2.V2TIMGroupMemberInfo;
import com.tencent.imsdk.v2.V2TIMManager;
import com.tencent.imsdk.v2.V2TIMMessage;
import com.tencent.imsdk.v2.V2TIMSimpleMsgListener;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.component.gift.store.GiftSendData;
import com.trtc.uikit.livekit.component.gift.store.GiftState;
import com.trtc.uikit.livekit.component.gift.store.GiftStore;
import com.trtc.uikit.livekit.component.gift.store.model.Gift;
import com.trtc.uikit.livekit.component.gift.store.model.GiftJson;
import com.trtc.uikit.livekit.component.gift.store.model.GiftUser;
import com.trtc.uikit.livekit.livestream.manager.api.LiveStreamLog;

import java.lang.ref.WeakReference;
import java.util.List;

/**
 * Responsible for handling the messaging service for sending and receiving gifts
 */
public class GiftIMService {
    private static final String TAG = "GiftIMService";

    private static final GsonNullStringAdapter mGsonNullStringAdapter = new GsonNullStringAdapter();

    public GiftIMService() {
        ReceiveGiftMsgListener mReceiveGiftMsgListener = new ReceiveGiftMsgListener(new WeakReference<>(this));
        V2TIMManager.getInstance().addSimpleMsgListener(mReceiveGiftMsgListener);
    }

    public void cleanGiftCacheList(String roomId) {
        GiftState giftState = GiftStore.sharedInstance().getGiftState(roomId);
        giftState.mGiftCacheList.getValue().clear();
    }

    private static class ReceiveGiftMsgListener extends V2TIMSimpleMsgListener {

        private final WeakReference<GiftIMService> mOuterClassReference;

        public ReceiveGiftMsgListener(WeakReference<GiftIMService> outerClassRef) {
            mOuterClassReference = outerClassRef;
        }

        @Override
        public void onRecvGroupCustomMessage(String msgID, String groupID, V2TIMGroupMemberInfo sender,
                                             byte[] customData) {
            GiftIMService outerClass = mOuterClassReference.get();
            if (outerClass != null) {
                if (groupID == null) {
                    return;
                }
                String customStr = new String(customData);
                Log.i(TAG, "customData :" + customStr);
                if (TextUtils.isEmpty(customStr)) {
                    Log.i(TAG, "onRecvGroupCustomMessage customData is empty");
                    return;
                }
                receiveGift(groupID, customStr);
            }
        }

        private void receiveGift(String roomId, String customStr) {
            try {
                Gson gson = new Gson();
                GiftJson json = gson.fromJson(customStr, GiftJson.class);
                if (!GiftConstants.VALUE_VERSION.equals(json.version)) {
                    Log.i(TAG, "protocol version is not match, ignore msg.");
                }
                if (GiftConstants.VALUE_BUSINESS_ID.equals(json.businessID)) {
                    GiftJson.Data data = json.data;

                    Gift gift = new Gift();
                    GiftUser sender = new GiftUser();
                    GiftUser receiver = new GiftUser();

                    if (data.gift != null) {
                        gift.giftId = data.gift.giftId;
                        gift.giftName = data.gift.giftName;
                        gift.imageUrl = data.gift.imageUrl;
                        gift.animationUrl = data.gift.animationUrl;
                        gift.price = data.gift.price;
                    }
                    if (data.sender != null) {
                        sender.userId = data.sender.userId;
                        sender.userName = data.sender.userName;
                        sender.avatarUrl = data.sender.avatarUrl;
                    }

                    if (data.receiver != null) {
                        receiver.userId = data.receiver.userId;
                        receiver.userName = data.receiver.userName;
                        receiver.avatarUrl = data.receiver.avatarUrl;
                    }
                    GiftIMService service = mOuterClassReference.get();
                    if (service == null) {
                        return;
                    }
                    GiftSendData sendData = new GiftSendData();
                    sendData.sender = sender;
                    sendData.receiver = receiver;
                    sendData.gift = gift;
                    sendData.giftCount = data.giftCount;
                    service.addGift(roomId, sendData);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public void sendGroupGiftMessage(String roomId, Gift gift, GiftUser receiver, int giftCount) {
        String data = getCusGiftMsgJsonStr(gift, receiver, giftCount);
        Log.i(TAG, "send gift: " + data);
        V2TIMManager.getInstance().sendGroupCustomMessage(data.getBytes(), roomId,
                V2TIMMessage.V2TIM_PRIORITY_NORMAL, new V2TIMValueCallback<V2TIMMessage>() {
                    @Override
                    public void onError(int error, String message) {
                        LiveStreamLog.error(TAG + " sendGroupCustomMessage failed:errorCode:" + "message:" + message);
                        ToastUtil.toastShortMessage(error + "," + message);
                    }

                    @Override
                    public void onSuccess(V2TIMMessage v2TIMMessage) {
                        final GiftUser sender = new GiftUser();
                        sender.userId = TUILogin.getUserId();
                        sender.userName = TUILogin.getNickName();
                        sender.avatarUrl = TUILogin.getFaceUrl();

                        GiftSendData sendData = new GiftSendData();
                        sendData.sender = sender;
                        sendData.receiver = receiver;
                        sendData.gift = gift;
                        sendData.giftCount = giftCount;
                        addGift(roomId, sendData);
                    }
                });
    }

    private static String getCusGiftMsgJsonStr(Gift gift, GiftUser receiver, int giftCount) {
        GiftJson sendJson = new GiftJson();
        sendJson.businessID = GiftConstants.VALUE_BUSINESS_ID;
        sendJson.platform = GiftConstants.VALUE_PLATFORM;
        sendJson.version = GiftConstants.VALUE_VERSION;

        GiftJson.Data data = new GiftJson.Data();
        data.giftCount = giftCount;

        GiftJson.Data.Gift giftJson = new GiftJson.Data.Gift();
        giftJson.giftId = gift.giftId;
        giftJson.giftName = gift.giftName;
        giftJson.imageUrl = gift.imageUrl;
        giftJson.animationUrl = gift.animationUrl;
        giftJson.price = gift.price;
        data.gift = giftJson;

        GiftJson.Data.User sender = new GiftJson.Data.User();
        sender.userId = TUILogin.getUserId();
        sender.userName = TUILogin.getNickName();
        sender.avatarUrl = TUILogin.getFaceUrl();
        sender.level = "0";
        data.sender = sender;

        GiftJson.Data.User giftReceiver = new GiftJson.Data.User();
        giftReceiver.userId = receiver.userId;
        giftReceiver.userName = receiver.userName;
        giftReceiver.avatarUrl = receiver.avatarUrl;
        data.receiver = giftReceiver;

        sendJson.data = data;

        Gson gson = new GsonBuilder().registerTypeAdapter(String.class, mGsonNullStringAdapter).create();
        return gson.toJson(sendJson);
    }

    private void addGift(String roomId, GiftSendData data) {
        if (data == null || !GiftStore.sharedInstance().hasCachedRoomId(roomId)) {
            return;
        }
        GiftState giftState = GiftStore.sharedInstance().getGiftState(roomId);
        List<GiftSendData> list = giftState.mGiftCacheList.getValue();
        list.add(data);
        giftState.mGiftCacheList.setValue(list);
    }
}
