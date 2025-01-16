package com.trtc.uikit.component.audiencelist.service;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.trtc.uikit.component.audiencelist.store.AudienceListState;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;

public class AudienceListObserver extends TUIRoomObserver {
    protected AudienceListState mAudienceListState;

    public AudienceListObserver(AudienceListState audienceListState) {
        mAudienceListState = audienceListState;
    }

    @Override
    public void onRoomUserCountChanged(String roomId, int userCount) {
        if (userCount > 0) {
            mAudienceListState.audienceCount.set(userCount - 1);
        }
    }

    @Override
    public void onRemoteUserEnterRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        if (userInfo.userId.equals(mAudienceListState.ownerId)) {
            return;
        }
        for (TUIRoomDefine.UserInfo info : mAudienceListState.audienceList.get()) {
            if (info.userId.equals(userInfo.userId)) {
                return;
            }
        }
        TUIRoomDefine.UserInfo audienceUser = new TUIRoomDefine.UserInfo();
        audienceUser.userId = userInfo.userId;
        audienceUser.userName = userInfo.userName;
        audienceUser.avatarUrl = userInfo.avatarUrl;
        audienceUser.userRole = userInfo.userRole;
        mAudienceListState.audienceList.add(audienceUser);
    }

    @Override
    public void onRemoteUserLeaveRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        Iterator<TUIRoomDefine.UserInfo> iterator = mAudienceListState.audienceList.get().iterator();
        while (iterator.hasNext()) {
            TUIRoomDefine.UserInfo audienceUser = iterator.next();
            if (audienceUser.userId.equals(userInfo.userId)) {
                iterator.remove();
                mAudienceListState.audienceList.notifyDataChanged();
                break;
            }
        }
    }

    @Override
    public void onUserInfoChanged(TUIRoomDefine.UserInfo userInfo, List<TUIRoomDefine.UserInfoModifyFlag> modifyFlag) {
        boolean hasChanged = false;
        LinkedHashSet<TUIRoomDefine.UserInfo> userList = mAudienceListState.audienceList.get();
        for (TUIRoomDefine.UserInfo info : userList) {
            if (TextUtils.equals(info.userId, userInfo.userId)) {
                if (modifyFlag.contains(TUIRoomDefine.UserInfoModifyFlag.USER_ROLE)) {
                    info.userRole = userInfo.userRole;
                    hasChanged = true;
                }
                break;
            }
        }
        if (hasChanged) {
            mAudienceListState.audienceList.notifyDataChanged();
        }
    }
}
