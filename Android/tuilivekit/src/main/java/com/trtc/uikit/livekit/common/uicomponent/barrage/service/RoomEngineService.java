package com.trtc.uikit.livekit.common.uicomponent.barrage.service;

import android.content.Context;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.barrage.model.TUIBarrage;
import com.trtc.uikit.livekit.common.uicomponent.barrage.service.IBarrageMessage.BarrageMessageDelegate;

public class RoomEngineService {

    private final TUIRoomEngine    mTUIRoomEngine = TUIRoomEngine.sharedInstance();
    private final Context          mContext;
    private BarrageMessageDelegate mDelegate;

    public RoomEngineService(Context context) {
        mContext = context;
    }

    public void setDelegate(BarrageMessageDelegate delegate) {
        mDelegate = delegate;
        if (delegate == null) {
            mTUIRoomEngine.removeObserver(mRoomObserver);
        } else {
            mTUIRoomEngine.addObserver(mRoomObserver);
        }
    }

    private final TUIRoomObserver mRoomObserver = new TUIRoomObserver() {
        @Override
        public void onRemoteUserEnterRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
            BarrageMessageDelegate delegate = mDelegate;
            if (delegate != null) {
                TUIBarrage barrage = new TUIBarrage();
                barrage.content = mContext.getString(R.string.livekit_entered_room);
                barrage.user.userId = userInfo.userId;
                barrage.user.userName = userInfo.userName;
                barrage.user.avatarUrl = userInfo.avatarUrl;
                barrage.user.level = "0";
                delegate.onReceivedBarrage(barrage);
            }
        }
    };

}
