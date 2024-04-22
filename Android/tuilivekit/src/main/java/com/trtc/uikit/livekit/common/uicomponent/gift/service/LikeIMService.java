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
import com.trtc.uikit.livekit.common.uicomponent.gift.model.LikeJson;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGiftUser;
import com.trtc.uikit.livekit.common.utils.GsonNullStringAdapter;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;

import java.lang.ref.WeakReference;

/**
 * Responsible for the message service of likes, sending, and receiving
 */
public class LikeIMService {
    private static final String TAG = "LikeIMService";

    private final String                 mRoomId;
    private final ReceiveLikeMsgListener mReceiveLikeMsgListener;
    private OnLikeMessageListener        mListener;

    private static final GsonNullStringAdapter mGsonNullStringAdapter = new GsonNullStringAdapter();

    public LikeIMService(String roomId) {
        this.mRoomId = roomId;
        mReceiveLikeMsgListener = new ReceiveLikeMsgListener(new WeakReference<>(this));
        initIMListener();
    }


    private void initIMListener() {
        V2TIMManager.getInstance().addSimpleMsgListener(mReceiveLikeMsgListener);
    }

    public void unInitImListener() {
        V2TIMManager.getInstance().removeSimpleMsgListener(mReceiveLikeMsgListener);
    }

    public void setListener(OnLikeMessageListener listener) {
        this.mListener = listener;
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
                if (groupID == null || !groupID.equals(mOuterClassReference.get().mRoomId)) {
                    return;
                }
                String customStr = new String(customData);
                LiveKitLog.info(TAG + " customData :" + customStr);
                if (TextUtils.isEmpty(customStr)) {
                    LiveKitLog.error(TAG + " onRecvGroupCustomMessage customData is empty");
                    return;
                }
                try {
                    Gson gson = new Gson();
                    LikeJson json = gson.fromJson(customStr, LikeJson.class);
                    if (!GiftConstants.VALUE_VERSION.equals(json.version)) {
                        LiveKitLog.info(TAG + " protocol version is not match, ignore msg.");
                    }
                    if (GiftConstants.VALUE_BUSINESS_ID_LIKE.equals(json.businessID)) {
                        LikeJson.Data data = json.data;
                        TUIGiftUser likeSender = new TUIGiftUser();
                        if (data.sender != null) {
                            likeSender.userId = data.sender.userId;
                            likeSender.userName = data.sender.userName;
                            likeSender.avatarUrl = data.sender.avatarUrl;
                            likeSender.level = data.sender.level;
                        }
                        OnLikeMessageListener listener = outerClass.mListener;
                        if (listener != null) {
                            listener.onReceiveLikeMessage(likeSender);
                        }
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
    }

    public void sendGroupLikeMessage(final GiftCallBack.ActionCallBack callback) {
        String data = getCusLikeMsgJsonStr();
        LiveKitLog.info(TAG + " send like: " + data);
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

    public interface OnLikeMessageListener {
        void onReceiveLikeMessage(TUIGiftUser sender);
    }
}
