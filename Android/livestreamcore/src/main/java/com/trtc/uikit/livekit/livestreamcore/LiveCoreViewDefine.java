package com.trtc.uikit.livekit.livestreamcore;

import android.view.View;

import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;

import java.util.List;

public class LiveCoreViewDefine {

    public enum LayoutMode {
        GRID_LAYOUT, FLOAT_LAYOUT, FREE_LAYOUT
    }

    public interface ConnectionObserver {
        void onConnectedUsersUpdated(List<UserInfo> userList, List<UserInfo> joinList, List<UserInfo> leaveList);

        void onUserConnectionRequest(UserInfo inviterUser);

        void onUserConnectionCancelled(UserInfo inviterUser);

        void onUserConnectionAccepted(UserInfo userInfo);

        void onUserConnectionRejected(UserInfo userInfo);

        void onUserConnectionTimeout(UserInfo userInfo);

        void onUserConnectionTerminated();

        void onUserConnectionExited(UserInfo userInfo);

        void onConnectedRoomsUpdated(List<ConnectionUser> roomList);

        void onCrossRoomConnectionRequest(ConnectionUser roomInfo);

        void onCrossRoomConnectionCancelled(ConnectionUser roomInfo);

        void onCrossRoomConnectionAccepted(ConnectionUser roomInfo);

        void onCrossRoomConnectionRejected(ConnectionUser roomInfo);

        void onCrossRoomConnectionTimeout(ConnectionUser inviter, ConnectionUser invitee);

        void onCrossRoomConnectionExited(ConnectionUser roomInfo);

        void onRoomDismissed(String roomId);
    }

    public interface VideoViewAdapter {
        View createCoGuestView(TUIRoomDefine.UserInfo userInfo);

        void updateCoGuestView(TUIRoomDefine.UserInfo userInfo, View coGuestView);

        View createCoHostView(ConnectionUser connectionUser);

        void updateCoHostView(ConnectionUser connectionUser, View coHostView);
    }
}
