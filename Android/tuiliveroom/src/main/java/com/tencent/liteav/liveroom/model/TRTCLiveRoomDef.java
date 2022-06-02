package com.tencent.liteav.liveroom.model;

public class TRTCLiveRoomDef {
    public static final int ROOM_STATUS_NONE     = 0;
    public static final int ROOM_STATUS_SINGLE   = 1;
    public static final int ROOM_STATUS_LINK_MIC = 2;
    public static final int ROOM_STATUS_PK       = 3;


    public static final class TRTCLiveRoomConfig {
        public boolean useCDNFirst;
        public String  cdnPlayDomain;

        public TRTCLiveRoomConfig(boolean useCDNFirst, String cdnPlayDomain) {
            this.useCDNFirst = useCDNFirst;
            this.cdnPlayDomain = cdnPlayDomain;
        }

        @Override
        public String toString() {
            return "TRTCLiveRoomConfig{"
                    + "useCDNFirst=" + useCDNFirst
                    + ", cdnPlayDomain='" + cdnPlayDomain + '\''
                    + '}';
        }
    }


    public static final class TRTCLiveUserInfo {
        public String userId;
        public String userName;
        public String userAvatar;

        @Override
        public String toString() {
            return "TRTCLiveUserInfo{"
                    + "userId='" + userId + '\''
                    + ", userName='" + userName + '\''
                    + ", userAvatar='" + userAvatar + '\''
                    + '}';
        }
    }


    public static final class TRTCCreateRoomParam {
        public String roomName;
        public String coverUrl;

        @Override
        public String toString() {
            return "TRTCCreateRoomParam{"
                    + "roomName='" + roomName + '\''
                    + ", coverUrl='" + coverUrl + '\''
                    + '}';
        }
    }


    public static final class TRTCLiveRoomInfo {
        public int    roomId;
        public String roomName;
        public String coverUrl;
        public String ownerId;
        public String ownerName;
        public String streamUrl;
        public int    roomStatus;
        public int    memberCount;

        @Override
        public String toString() {
            return "TRTCLiveRoomInfo{"
                    + "roomId=" + roomId
                    + ", roomName='" + roomName + '\''
                    + ", coverUrl='" + coverUrl + '\''
                    + ", ownerId='" + ownerId + '\''
                    + ", ownerName='" + ownerName + '\''
                    + ", streamUrl='" + streamUrl + '\''
                    + ", roomStatus=" + roomStatus
                    + ", memberCount=" + memberCount
                    + '}';
        }
    }
}