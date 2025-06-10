package com.trtc.uikit.livekit.component.audiencelist.service;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.audiencelist.store.AudienceListState;

import java.util.LinkedHashSet;
import java.util.Set;

public class AudienceListService {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getComponentLogger("AudienceListService");

    public final AudienceListState mAudienceListState = new AudienceListState();

    public void initRoomInfo(TUIRoomDefine.RoomInfo roomInfo) {
        mAudienceListState.roomId = roomInfo.roomId;
        mAudienceListState.ownerId = roomInfo.ownerId;
    }

    public void getAudienceList() {
        TUIRoomEngine.sharedInstance().getUserList(0, new TUIRoomDefine.GetUserListCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserListResult userListResult) {
                if (!userListResult.userInfoList.isEmpty()) {
                    mAudienceListState.audienceList.getValue().clear();
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
                    mAudienceListState.audienceList.getValue().addAll(userInfoSet);
                    mAudienceListState.audienceList.setValue(mAudienceListState.audienceList.getValue());
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("getUserList failed:error:" + error + ",errorCode:" + error.getValue() +
                        "message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }
}
