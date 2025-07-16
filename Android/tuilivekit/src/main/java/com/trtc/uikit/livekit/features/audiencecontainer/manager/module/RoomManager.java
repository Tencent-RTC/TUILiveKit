package com.trtc.uikit.livekit.features.audiencecontainer.manager.module;

import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.KickedOutOfRoomReason.BY_LOGGED_ON_OTHER_DEVICE;
import static com.trtc.uikit.livekit.features.audiencecontainer.manager.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.features.audiencecontainer.manager.Constants.EVENT_SUB_KEY_DESTROY_AUDIENCE_CONTAINER;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.api.ILiveService;
import com.trtc.uikit.livekit.features.audiencecontainer.state.AudienceState;

public class RoomManager extends BaseManager {
    public RoomManager(AudienceState state, ILiveService service) {
        super(state, service);
    }

    public void updateRoomState(TUIRoomDefine.RoomInfo roomInfo) {
        mRoomState.roomInfo = roomInfo;
    }

    public void onKickedOutOfRoom(String roomId, TUIRoomDefine.KickedOutOfRoomReason reason) {
        if (mCoreState.userState.selfInfo.getValue().userRole == TUIRoomDefine.Role.ROOM_OWNER) {
            return;
        }
        if (reason != null && BY_LOGGED_ON_OTHER_DEVICE != reason) {
            ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                    .getString(R.string.common_kicked_out_of_room_by_owner));
            TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_DESTROY_AUDIENCE_CONTAINER, null);
        }
    }

    public void onLiveVideoLayoutChanged(String roomId, int width, int height) {
        if (!mRoomState.roomInfo.roomId.equals(roomId)) {
            return;
        }
        boolean isLandscape = width >= height;
        mRoomState.videoStreamIsLandscape.setValue(isLandscape);
    }
}
