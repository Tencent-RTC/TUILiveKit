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

        void updateCoGuestView(TUIRoomDefine.UserInfo userInfo, List<UserInfoModifyFlag> modifyFlag, View coGuestView);

        View createCoHostView(CoHostUser coHostUser);

        void updateCoHostView(CoHostUser coHostUser, List<UserInfoModifyFlag> modifyFlag, View coHostView);
    }

    public static class CoHostUser {
        public ConnectionUser connectionUser;
        public boolean        hasAudioStream;
        public boolean        hasVideoStream;

    }

    public enum UserInfoModifyFlag {
        NONE(0x00),
        USER_ROLE(0x01),
        NAME_CARD(0x01 << 1),
        HAS_VIDEO_STREAM(0x01 << 2),
        HAS_AUDIO_STREAM(0x01 << 3);
        final int mValue;

        UserInfoModifyFlag(int value) {
            mValue = value;
        }

        public int getValue() {
            return mValue;
        }

        public static UserInfoModifyFlag fromInt(int value) {
            for (UserInfoModifyFlag flag : UserInfoModifyFlag.values()) {
                if (flag.mValue == value) {
                    return flag;
                }
            }
            return NONE;
        }
    }
}
