package com.trtc.uikit.livekit.common.uicomponent.gift.service;

import android.text.TextUtils;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.tencent.imsdk.v2.V2TIMGroupMemberInfo;
import com.tencent.imsdk.v2.V2TIMManager;
import com.tencent.imsdk.v2.V2TIMMessage;
import com.tencent.imsdk.v2.V2TIMSimpleMsgListener;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.GiftJson;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGift;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGiftUser;
import com.trtc.uikit.livekit.common.utils.GsonNullStringAdapter;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;

import java.lang.ref.WeakReference;

/**
 * Responsible for handling the messaging service for sending and receiving gifts
 */
public class GiftIMService {
    private static final String TAG = "GiftIMService";

    private final String                 mRoomId;
    private final ReceiveGiftMsgListener mReceiveGiftMsgListener;
    private       OnGiftMessageListener  mListener;

    private static final GsonNullStringAdapter mGsonNullStringAdapter = new GsonNullStringAdapter();

    public GiftIMService(String roomId) {
        this.mRoomId = roomId;
        mReceiveGiftMsgListener = new ReceiveGiftMsgListener(new WeakReference<>(this));
        initIMListener();
    }


    private void initIMListener() {
        V2TIMManager.getInstance().addSimpleMsgListener(mReceiveGiftMsgListener);
    }

    public void unInitImListener() {
        V2TIMManager.getInstance().removeSimpleMsgListener(mReceiveGiftMsgListener);
    }

    public void setListener(OnGiftMessageListener listener) {
        this.mListener = listener;
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
                if (groupID == null || !groupID.equals(mOuterClassReference.get().mRoomId)) {
                    return;
                }
                String customStr = new String(customData);
                LiveKitLog.info(TAG + " customData :" + customStr);
                if (TextUtils.isEmpty(customStr)) {
                    LiveKitLog.error(TAG + " onRecvGroupCustomMessage customData is empty");
                    return;
                }
                receiveGift(customStr);
            }
        }

        private void receiveGift(String customStr) {
            try {
                Gson gson = new Gson();
                GiftJson json = gson.fromJson(customStr, GiftJson.class);
                if (!GiftConstants.VALUE_VERSION.equals(json.version)) {
                    LiveKitLog.error(TAG + " protocol version is not match, ignore msg.");
                }
                if (GiftConstants.VALUE_BUSINESS_ID.equals(json.businessID)) {
                    GiftJson.Data data = json.data;

                    TUIGift gift = new TUIGift();
                    TUIGiftUser sender = new TUIGiftUser();
                    TUIGiftUser receiver = new TUIGiftUser();

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
                        sender.level = data.sender.level;
                    }

                    if (data.receiver != null) {
                        receiver.userId = data.receiver.userId;
                        receiver.userName = data.receiver.userName;
                        receiver.avatarUrl = data.receiver.avatarUrl;
                        receiver.level = data.receiver.level;
                    }
                    int count = data.giftCount;
                    GiftIMService outerClass = mOuterClassReference.get();
                    if (outerClass == null) {
                        return;
                    }
                    OnGiftMessageListener listener = outerClass.mListener;
                    if (listener != null) {
                        listener.onReceiveGiftMessage(gift, count, sender, receiver);
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public void sendGroupGiftMessage(TUIGift gift, TUIGiftUser receiver, int giftCount,
                                     final GiftCallBack.ActionCallBack callback) {
        String data = getCusGiftMsgJsonStr(gift, receiver, giftCount);
        LiveKitLog.info(TAG + " send gift: " + data);
        V2TIMManager.getInstance().sendGroupCustomMessage(data.getBytes(), mRoomId,
                V2TIMMessage.V2TIM_PRIORITY_NORMAL, new V2TIMValueCallback<V2TIMMessage>() {
                    @Override
                    public void onError(int i, String s) {
                        if (callback != null) {
                            callback.onCallback(i, s);
                        }
                    }

                    @Override
                    public void onSuccess(V2TIMMessage v2TIMMessage) {
                        if (callback != null) {
                            callback.onCallback(0, "send group message success.");
                        }
                    }
                });
    }

    private static String getCusGiftMsgJsonStr(TUIGift gift, TUIGiftUser receiver, int giftCount) {
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
        giftReceiver.level = receiver.level;
        data.receiver = giftReceiver;

        sendJson.data = data;

        Gson gson = new GsonBuilder().registerTypeAdapter(String.class, mGsonNullStringAdapter).create();
        return gson.toJson(sendJson);
    }

    public interface OnGiftMessageListener {
        void onReceiveGiftMessage(TUIGift gift, int giftCount, TUIGiftUser sender, TUIGiftUser receiver);
    }
}
