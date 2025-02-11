package com.trtc.uikit.component.audiencelist.service;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.trtc.uikit.component.audiencelist.store.AudienceListState;

import java.util.LinkedHashSet;
import java.util.Set;

public class AudienceListService {
    public final AudienceListState mAudienceListState = new AudienceListState();

    public void initRoomInfo(String roomId) {
        mAudienceListState.roomId = roomId;
        TUIRoomEngine.sharedInstance().fetchRoomInfo(roomId, TUIRoomDefine.RoomType.LIVE,
                new TUIRoomDefine.GetRoomInfoCallback() {
                    @Override
                    public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                        mAudienceListState.ownerId = roomInfo.ownerId;
                        getAudienceList();
                    }

                    @Override
                    public void onError(TUICommonDefine.Error error, String message) {

                    }
                });
    }

    public void getAudienceList() {
        TUIRoomEngine.sharedInstance().getUserList(0, new TUIRoomDefine.GetUserListCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserListResult userListResult) {
                if (!userListResult.userInfoList.isEmpty()) {
                    mAudienceListState.audienceList.get().clear();
                    Set<TUIRoomDefine.UserInfo> userInfoSet = new LinkedHashSet<>();
                    for (TUIRoomDefine.UserInfo userInfo : userListResult.userInfoList) {
                        if (userInfo.userId.equals(mAudienceListState.ownerId)) {
                            continue;
                        }
                        if (userInfoSet.size() >= AudienceListState.ROOM_MAX_SHOW_USER_COUNT) {
                            break;
                        }
                        userInfoSet.add(userInfo);
                    }
                    mAudienceListState.audienceList.addAll(userInfoSet);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
            }
        });
    }
}
