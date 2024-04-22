package com.trtc.uikit.livekit.common.uicomponent.barrage.service;

import static com.trtc.uikit.livekit.common.uicomponent.barrage.service.BarrageConstants.VALUE_BUSINESS_ID;
import static com.trtc.uikit.livekit.common.uicomponent.barrage.service.BarrageConstants.VALUE_PLATFORM;
import static com.trtc.uikit.livekit.common.uicomponent.barrage.service.BarrageConstants.VALUE_VERSION;

import android.text.TextUtils;

import com.google.gson.Gson;
import com.tencent.imsdk.v2.V2TIMGroupMemberInfo;
import com.tencent.imsdk.v2.V2TIMManager;
import com.tencent.imsdk.v2.V2TIMMessage;
import com.tencent.imsdk.v2.V2TIMMessageManager;
import com.tencent.imsdk.v2.V2TIMSimpleMsgListener;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.trtc.uikit.livekit.common.uicomponent.barrage.model.TUIBarrage;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;


public class BarrageIMService implements IBarrageMessage {
    private static final String TAG = "BarrageIMService";

    private SimpleListener          mSimpleListener;
    private final String            mRoomId;
    private BarrageMessageDelegate  mDelegate;

    public BarrageIMService(String roomId) {
        mRoomId = roomId;
    }

    @Override
    public void setDelegate(BarrageMessageDelegate delegate) {
        mDelegate = delegate;
        if (delegate == null) {
            V2TIMManager.getInstance().setGroupListener(null);
            V2TIMManager.getInstance().removeSimpleMsgListener(mSimpleListener);
        } else {
            V2TIMMessageManager messageManager = V2TIMManager.getMessageManager();
            if (mSimpleListener == null) {
                mSimpleListener = new SimpleListener();
            }
            V2TIMManager.getInstance().addSimpleMsgListener(mSimpleListener);
        }
    }

    @Override
    public void sendBarrage(TUIBarrage barrage, final BarrageSendCallBack callback) {
        if (TextUtils.isEmpty(barrage.content)) {
            LiveKitLog.debug(TAG + " sendBarrage data is empty");
            return;
        }
        String text = getTextMsgJsonStr(barrage);
        LiveKitLog.debug(TAG + " sendBarrage:" + text);
        V2TIMManager.getInstance().sendGroupCustomMessage(text.getBytes(), mRoomId, V2TIMMessage.V2TIM_PRIORITY_HIGH,
                new V2TIMValueCallback<V2TIMMessage>() {
                    @Override
                    public void onSuccess(V2TIMMessage v2TIMMessage) {
                        if (callback != null) {
                            callback.onSuccess(barrage);
                            LiveKitLog.debug(TAG + " sendGroupCustomMessage success");
                        }
                    }

                    @Override
                    public void onError(int i, String s) {
                        LiveKitLog.error(TAG + " sendGroupCustomMessage error " + i + " errorMessage:" + s);
                        if (callback != null) {
                            callback.onFailed(i, s);
                        }
                    }
                });
    }

    private class SimpleListener extends V2TIMSimpleMsgListener {
        @Override
        public void onRecvGroupTextMessage(String msgID, String groupID, V2TIMGroupMemberInfo sender, String text) {
            LiveKitLog.debug(TAG + " onRecvGroupCustomMessage: msgID = " + msgID + " , groupID = " + groupID
                    + " , mGroupId = " + mRoomId + " , sender = " + sender + " , text = " + text);
            if (groupID == null || !groupID.equals(mRoomId)) {
                return;
            }
            if (TextUtils.isEmpty(text)) {
                LiveKitLog.debug(TAG + " onRecvGroupCustomMessage customData is empty");
                return;
            }
            TUIBarrage barrage = new TUIBarrage();
            barrage.content = text;
            barrage.user.userId = sender.getUserID();
            barrage.user.userName = sender.getNickName();
            barrage.user.avatarUrl = sender.getFaceUrl();
            barrage.user.level = "0";

            if (mDelegate != null) {
                mDelegate.onReceivedBarrage(barrage);
            }
        }

        @Override
        public void onRecvGroupCustomMessage(String msgID, String groupID,
                                             V2TIMGroupMemberInfo sender, byte[] customData) {
            LiveKitLog.debug(TAG + " onRecvGroupCustomMessage: msgID = " + msgID + " , groupID = " + groupID
                    + " , mGroupId = " + mRoomId + " , sender = " + sender);
            if (groupID == null || !groupID.equals(mRoomId)) {
                return;
            }
            if (customData == null) {
                LiveKitLog.error(TAG + " onRecvGroupCustomMessage customData is empty");
                return;
            }
            try {
                String info = new String(customData);
                Gson gson = new Gson();
                BarrageJson barrageJson = gson.fromJson(info, BarrageJson.class);
                LiveKitLog.debug(TAG + " " + barrageJson);
                TUIBarrage barrage = new TUIBarrage();
                if (barrageJson.data != null) {
                    barrage.content = barrageJson.data.content;
                    if (barrageJson.data.user != null) {
                        barrage.user.userId = barrageJson.data.user.userId;
                        barrage.user.userName = barrageJson.data.user.userName;
                        barrage.user.avatarUrl = barrageJson.data.user.avatarUrl;
                        barrage.user.level = barrageJson.data.user.level;
                    }
                }
                if (mDelegate != null) {
                    mDelegate.onReceivedBarrage(barrage);
                }
            } catch (Exception e) {
                LiveKitLog.error(TAG + " " + e.getLocalizedMessage());
            }
        }
    }

    public static String getTextMsgJsonStr(TUIBarrage barrage) {
        if (barrage == null) {
            return null;
        }
        BarrageJson sendJson = new BarrageJson();
        sendJson.businessID = VALUE_BUSINESS_ID;
        sendJson.platform = VALUE_PLATFORM;
        sendJson.version = VALUE_VERSION;

        BarrageJson.Data data = new BarrageJson.Data();
        data.content = barrage.content;
        sendJson.data = data;

        BarrageJson.Data.User user = new BarrageJson.Data.User();
        user.userName = barrage.user.userName;
        user.userId = barrage.user.userId;
        user.avatarUrl = barrage.user.avatarUrl;
        user.level = barrage.user.level;
        sendJson.data.user = user;

        Gson gson = new Gson();
        return gson.toJson(sendJson);
    }
}
