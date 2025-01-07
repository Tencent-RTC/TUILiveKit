package com.trtc.uikit.livekit.livestreamcore;

import android.graphics.Rect;
import android.view.View;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleInfo;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleUser;
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

    public interface BattleObserver {
        void onBattleStarted(BattleInfo battleInfo);

        void onBattleEnded(BattleInfo battleInfo);

        void onUserJoinBattle(String battleId, BattleUser battleUser);

        void onUserExitBattle(String battleId, BattleUser battleUser);

        void onBattleScoreChanged(String battleId, List<BattleUser> battleUserList);

        void onBattleRequestReceived(String battleId, BattleUser inviter, BattleUser invitee);

        void onBattleRequestCancelled(String battleId, BattleUser inviter, BattleUser invitee);

        void onBattleRequestTimeout(String battleId, BattleUser inviter, BattleUser invitee);

        void onBattleRequestAccept(String battleId, BattleUser inviter, BattleUser invitee);

        void onBattleRequestReject(String battleId, BattleUser inviter, BattleUser invitee);
    }

    public interface BattleRequestCallback {
        void onSuccess(String battleId, List<String> requestedUserIdList);

        void onError(TUICommonDefine.Error error, String message);
    }


    public interface VideoViewAdapter {
        View createCoGuestView(TUIRoomDefine.UserInfo userInfo);

        void updateCoGuestView(View coGuestView, TUIRoomDefine.UserInfo userInfo, List<UserInfoModifyFlag> modifyFlag);

        View createCoHostView(CoHostUser coHostUser);

        void updateCoHostView(View coHostView, CoHostUser coHostUser, List<UserInfoModifyFlag> modifyFlag);

        View createBattleView(BattleUser battleUser);

        void updateBattleView(View battleView, BattleUser battleUser);

        View createBattleContainerView();

        void updateBattleContainerView(View battleContainnerView, List<BattleUserViewModel> userInfos);
    }

    public static class BattleUserViewModel {
        public BattleUser battleUser;
        public Rect       rect = new Rect();
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
