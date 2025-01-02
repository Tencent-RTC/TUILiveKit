package com.trtc.uikit.component.barrage.service;

import android.text.TextUtils;
import android.util.Log;

import com.google.gson.Gson;
import com.tencent.imsdk.v2.V2TIMGroupMemberInfo;
import com.tencent.imsdk.v2.V2TIMManager;
import com.tencent.imsdk.v2.V2TIMMessage;
import com.tencent.imsdk.v2.V2TIMSimpleMsgListener;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.trtc.uikit.component.barrage.store.BarrageState;
import com.trtc.uikit.component.barrage.store.BarrageStore;
import com.trtc.uikit.component.barrage.store.model.Barrage;

import java.util.List;

public class BarrageIMService {
    private static final String TAG = "BarrageIMService";

    private int mMaxBarrageCount = 1000;

    public BarrageIMService() {
        SimpleListener listener = new SimpleListener();
        V2TIMManager.getInstance().addSimpleMsgListener(listener);
    }

    public void setMaxBarrageCount(int count) {
        mMaxBarrageCount = count > 0 ? count : mMaxBarrageCount;
    }

    public void sendBarrage(String roomId, Barrage barrage) {
        Log.i(TAG, "sendBarrage:" + new Gson().toJson(barrage));
        if (TextUtils.isEmpty(barrage.content)) {
            return;
        }
        V2TIMManager.getInstance().sendGroupTextMessage(barrage.content, roomId, V2TIMMessage.V2TIM_PRIORITY_HIGH,
                new V2TIMValueCallback<V2TIMMessage>() {
                    @Override
                    public void onSuccess(V2TIMMessage v2TIMMessage) {
                        Log.i(TAG, "sendGroupTextMessage success");
                        insertBarrages(roomId, barrage);
                    }

                    @Override
                    public void onError(int i, String s) {
                        Log.i(TAG, "sendGroupTextMessage error " + i + " errorMessage:" + s);
                    }
                });
    }

    public void insertBarrages(String roomId, Barrage... barrages) {
        if (barrages == null || !BarrageStore.sharedInstance().hasCachedRoomId(roomId)) {
            return;
        }
        BarrageState barrageState = BarrageStore.sharedInstance().getBarrageState(roomId);
        List<Barrage> list = barrageState.mBarrageCacheList.get();
        int count = barrageState.mBarrageTotalCount.get();
        for (Barrage barrage : barrages) {
            if (barrage != null) {
                list.add(barrage);
                count++;
            }
        }
        if (list.size() > mMaxBarrageCount) {
            list.subList(0, list.size() - mMaxBarrageCount).clear();
        }
        barrageState.mBarrageTotalCount.set(count);
        barrageState.mBarrageCacheList.set(list);
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
            barrage.user.level = "32";
            insertBarrages(groupID, barrage);
        }
    }
}
