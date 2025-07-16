package com.trtc.uikit.livekit.component.barrage.service;

import android.text.TextUtils;
import android.util.Log;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.tencent.imsdk.v2.V2TIMGroupMemberInfo;
import com.tencent.imsdk.v2.V2TIMManager;
import com.tencent.imsdk.v2.V2TIMMessage;
import com.tencent.imsdk.v2.V2TIMSimpleMsgListener;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.tencent.qcloud.tuicore.interfaces.TUICallback;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.barrage.store.BarrageState;
import com.trtc.uikit.livekit.component.barrage.store.BarrageStore;
import com.trtc.uikit.livekit.component.barrage.store.model.Barrage;

import java.util.List;

public class BarrageIMService {
    private static final String TAG = "BarrageIMService";

    private int mMaxBarrageCount = 1000;

    public BarrageIMService() {
        V2TIMManager.getInstance().addSimpleMsgListener(new SimpleListener());
        TUIRoomEngine.sharedInstance().addObserver(new RoomEngineObserver());
    }

    public void setMaxBarrageCount(int count) {
        mMaxBarrageCount = count > 0 ? count : mMaxBarrageCount;
    }

    public void sendBarrage(String roomId, Barrage barrage, TUICallback callback) {
        Log.i(TAG, "sendBarrage:" + new Gson().toJson(barrage));
        if (TextUtils.isEmpty(barrage.content)) {
            return;
        }
        V2TIMManager.getInstance().sendGroupTextMessage(barrage.content, roomId, V2TIMMessage.V2TIM_PRIORITY_LOW,
                new V2TIMValueCallback<V2TIMMessage>() {
                    @Override
                    public void onSuccess(V2TIMMessage v2TIMMessage) {
                        Log.i(TAG, "sendGroupTextMessage success");
                        if (callback != null) {
                            callback.onSuccess();
                        }
                        insertBarrages(roomId, barrage);
                    }

                    @Override
                    public void onError(int code, String s) {
                        Log.e(TAG, "sendGroupTextMessage error " + code + " errorMessage:" + s);
                        if (callback != null) {
                            callback.onError(code, s);
                        }
                    }
                });
    }

    public void insertBarrages(String roomId, Barrage... barrages) {
        if (barrages == null || !BarrageStore.sharedInstance().hasCachedRoomId(roomId)) {
            return;
        }
        BarrageState barrageState = BarrageStore.sharedInstance().getBarrageState(roomId);
        List<Barrage> list = barrageState.mBarrageCacheList.getValue();
        int count = barrageState.mBarrageTotalCount.getValue();
        for (Barrage barrage : barrages) {
            if (barrage != null) {
                list.add(barrage);
                count++;
            }
        }
        if (list.size() > mMaxBarrageCount) {
            list.subList(0, list.size() - mMaxBarrageCount).clear();
        }
        barrageState.mBarrageTotalCount.setValue(count);
        barrageState.mBarrageCacheList.setValue(list);
    }

    private class SimpleListener extends V2TIMSimpleMsgListener {
        @Override
        public void onRecvGroupTextMessage(String msgID, String groupID, V2TIMGroupMemberInfo sender, String text) {
            Log.i(TAG, "onRecvGroupTextMessage: msgID = " + msgID + " , groupID = " + groupID
                    + " , groupID = " + groupID + " , sender = " + sender
                    + " , text = " + text);
            if (TextUtils.isEmpty(text)) {
                return;
            }
            Barrage barrage = new Barrage();
            barrage.content = text;
            barrage.user.userId = sender.getUserID();
            barrage.user.userName = sender.getNickName();
            barrage.user.avatarUrl = sender.getFaceUrl();
            insertBarrages(groupID, barrage);
        }
    }

    private class RoomEngineObserver extends TUIRoomObserver {
        @Override
        public void onRemoteUserEnterRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
            if (TextUtils.isEmpty(roomId) || userInfo == null) {
                return;
            }
            Barrage barrage = new Barrage();
            barrage.content = ContextProvider.getApplicationContext().getString(R.string.common_entered_room);
            barrage.user.userId = userInfo.userId;
            barrage.user.userName = userInfo.userName;
            barrage.user.avatarUrl = userInfo.avatarUrl;
            insertBarrages(roomId, barrage);
        }
    }
}
