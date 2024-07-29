package com.trtc.uikit.livekit.common.uicomponent.barrage.service;

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

    private       SimpleListener         mSimpleListener;
    private       BarrageMessageDelegate mDelegate;
    private final String                 mRoomId;

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
                V2TIMManager.getInstance().addSimpleMsgListener(mSimpleListener);
            }
        }
    }

    @Override
    public void sendBarrage(TUIBarrage barrage, final BarrageSendCallBack callback) {
        LiveKitLog.info(TAG + " sendBarrage:" + new Gson().toJson(barrage));
        if (TextUtils.isEmpty(barrage.content)) {
            return;
        }
        V2TIMManager.getInstance().sendGroupTextMessage(barrage.content, mRoomId, V2TIMMessage.V2TIM_PRIORITY_HIGH,
                new V2TIMValueCallback<V2TIMMessage>() {
                    @Override
                    public void onSuccess(V2TIMMessage v2TIMMessage) {
                        if (callback != null) {
                            callback.onSuccess(barrage);
                            LiveKitLog.info(TAG + " sendGroupTextMessage success");
                        }
                    }

                    @Override
                    public void onError(int i, String s) {
                        LiveKitLog.error(TAG + " sendGroupTextMessage error " + i + " errorMessage:" + s);
                        if (callback != null) {
                            callback.onFailed(i, s);
                        }
                    }
                });
    }

    private class SimpleListener extends V2TIMSimpleMsgListener {
        @Override
        public void onRecvGroupTextMessage(String msgID, String groupID, V2TIMGroupMemberInfo sender, String text) {
            LiveKitLog.info(TAG + " onRecvGroupTextMessage: msgID = " + msgID + " , groupID = " + groupID
                    + " , mRoomId = " + mRoomId + " , sender = " + sender
                    + " , text = " + text);
            if (!TextUtils.equals(groupID, mRoomId) || TextUtils.isEmpty(text)) {
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
    }
}
