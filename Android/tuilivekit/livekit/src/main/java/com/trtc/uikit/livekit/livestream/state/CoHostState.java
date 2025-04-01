package com.trtc.uikit.livekit.livestream.state;

import androidx.annotation.Nullable;
import androidx.lifecycle.MutableLiveData;

import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;

import java.util.ArrayList;
import java.util.List;

public class CoHostState {
    public       String                                recommendedCursor         = "";
    public final MutableLiveData<List<ConnectionUser>> recommendUsers            = new MutableLiveData<>(new ArrayList<>());
    public final MutableLiveData<List<ConnectionUser>> connectedUsers            = new MutableLiveData<>(new ArrayList<>());
    public final MutableLiveData<List<ConnectionUser>> sentConnectionRequests    =
            new MutableLiveData<>(new ArrayList<>());
    public final MutableLiveData<ConnectionUser>       receivedConnectionRequest = new MutableLiveData<>();

    public void reset() {
        recommendUsers.getValue().clear();
        connectedUsers.getValue().clear();
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
