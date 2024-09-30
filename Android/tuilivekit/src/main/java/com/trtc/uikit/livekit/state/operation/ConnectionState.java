package com.trtc.uikit.livekit.state.operation;

import androidx.annotation.Nullable;

import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.trtc.tuikit.common.livedata.LiveData;

import java.util.ArrayList;
import java.util.List;

public class ConnectionState {
    public       String                         recommendedCursor         = "";
    public final LiveData<List<ConnectionUser>> recommendUsers            = new LiveData<>(new ArrayList<>());
    public final LiveData<List<ConnectionUser>> connectedUsers            = new LiveData<>(new ArrayList<>());
    public final LiveData<List<ConnectionUser>> sentConnectionRequests    =
            new LiveData<>(new ArrayList<>());
    public final LiveData<ConnectionUser>       receivedConnectionRequest = new LiveData<>();

    public void reset() {
        recommendUsers.get().clear();
        connectedUsers.get().clear();
    }

    public enum ConnectionStatus {
        UNKNOWN(0),
        INVITING(1),
        CONNECTED(2);

        private final int mValue;

        private ConnectionStatus(int value) {
            this.mValue = value;
        }
    }


    public static class ConnectionUser {
        public String           roomId;
        public String           userId;
        public String           userName;
        public String           avatarUrl;
        public long             joinConnectionTime;
        public ConnectionStatus connectionStatus;

        public ConnectionUser(TUILiveListManager.LiveInfo liveUser) {
            this.roomId = liveUser.roomInfo.roomId;
            this.userId = liveUser.roomInfo.ownerId;
            this.userName = liveUser.roomInfo.ownerName;
            this.avatarUrl = liveUser.roomInfo.ownerAvatarUrl;
            this.joinConnectionTime = 0;
            this.connectionStatus = ConnectionStatus.UNKNOWN;
        }

        public ConnectionUser(TUILiveConnectionManager.ConnectionUser connectionUser,
                              ConnectionStatus connectionStatus) {
            this.roomId = connectionUser.roomId;
            this.userId = connectionUser.userId;
            this.userName = connectionUser.userName;
            this.avatarUrl = connectionUser.avatarUrl;
            this.joinConnectionTime = connectionUser.joinConnectionTime;
            this.connectionStatus = connectionStatus;
        }

        @Override
        public boolean equals(@Nullable Object obj) {
            if (obj instanceof ConnectionUser) {
                return this.userId.equals(((ConnectionUser) obj).userId);
            }
            return false;
        }
    }
}
