package com.trtc.uikit.livekit.component.gift.service;

import android.text.TextUtils;
import android.util.Log;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.tencent.imsdk.v2.V2TIMGroupMemberInfo;
import com.tencent.imsdk.v2.V2TIMManager;
import com.tencent.imsdk.v2.V2TIMMessage;
import com.tencent.imsdk.v2.V2TIMSimpleMsgListener;
import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.uikit.livekit.component.gift.store.GiftStore;
import com.trtc.uikit.livekit.component.gift.store.LikeState;
import com.trtc.uikit.livekit.component.gift.store.model.GiftUser;
import com.trtc.uikit.livekit.component.gift.store.model.LikeJson;

import java.lang.ref.WeakReference;

/**
 * Responsible for the message service of likes, sending, and receiving
 */
public class LikeIMService {
    private static final String TAG = "LikeIMService";

    private static final GsonNullStringAdapter mGsonNullStringAdapter = new GsonNullStringAdapter();

    public LikeIMService() {
        ReceiveLikeMsgListener listener = new ReceiveLikeMsgListener(new WeakReference<>(this));
        V2TIMManager.getInstance().addSimpleMsgListener(listener);
    }

    public void likeByLocal(String roomId) {
        LikeState state = GiftStore.sharedInstance().getLikeState(roomId);
        state.mLikeAnimationTrigger.setValue(true);
    }

    private static class ReceiveLikeMsgListener extends V2TIMSimpleMsgListener {

        private final WeakReference<LikeIMService> mOuterClassReference;

        public ReceiveLikeMsgListener(WeakReference<LikeIMService> outerClassRef) {
            mOuterClassReference = outerClassRef;
        }

        @Override
        public void onRecvGroupCustomMessage(String msgID, String groupID, V2TIMGroupMemberInfo sender,
                                             byte[] customData) {
            LikeIMService outerClass = mOuterClassReference.get();
            if (outerClass != null) {
                if (groupID == null) {
                    return;
                }
                String customStr = new String(customData);
                Log.i(TAG, " customData :" + customStr);
                if (TextUtils.isEmpty(customStr)) {
                    Log.i(TAG, " onRecvGroupCustomMessage customData is empty");
                    return;
                }
                try {
                    Gson gson = new Gson();
                    LikeJson json = gson.fromJson(customStr, LikeJson.class);
                    if (!GiftConstants.VALUE_VERSION.equals(json.version)) {
                        Log.i(TAG, " protocol version is not match, ignore msg.");
                    }
                    if (GiftConstants.VALUE_BUSINESS_ID_LIKE.equals(json.businessID)) {
                        LikeJson.Data data = json.data;
                        GiftUser likeSender = new GiftUser();
                        if (data.sender != null) {
                            likeSender.userId = data.sender.userId;
                            likeSender.userName = data.sender.userName;
                            likeSender.avatarUrl = data.sender.avatarUrl;
                        }
                        if (GiftStore.sharedInstance().hasCachedRoomId(groupID)) {
                            LikeState state = GiftStore.sharedInstance().getLikeState(groupID);
                            state.mLikeReceivedTotalCount.setValue(state.mLikeReceivedTotalCount.getValue() + 1);
                            state.mLikeAnimationTrigger.setValue(true);
                        }
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
    }

    public void sendGroupLikeMessage(String roomId) {
        String data = getCusLikeMsgJsonStr();
        Log.i(TAG, " send like: " + data);
        V2TIMManager.getInstance().sendGroupCustomMessage(
                data.getBytes(), roomId, V2TIMMessage.V2TIM_PRIORITY_LOW, null);
    }

    private static String getCusLikeMsgJsonStr() {
        LikeJson sendJson = new LikeJson();
        sendJson.businessID = GiftConstants.VALUE_BUSINESS_ID_LIKE;
        sendJson.platform = GiftConstants.VALUE_PLATFORM;
        sendJson.version = GiftConstants.VALUE_VERSION;

        LikeJson.Data.User sender = new LikeJson.Data.User();
        sender.userId = TUILogin.getUserId();
        sender.userName = TUILogin.getNickName();
        sender.avatarUrl = TUILogin.getFaceUrl();
        sender.level = "0";

        LikeJson.Data data = new LikeJson.Data();
        data.sender = sender;
        sendJson.data = data;
        Gson gson = new GsonBuilder().registerTypeAdapter(String.class, mGsonNullStringAdapter).create();
        return gson.toJson(sendJson);
    }
}
