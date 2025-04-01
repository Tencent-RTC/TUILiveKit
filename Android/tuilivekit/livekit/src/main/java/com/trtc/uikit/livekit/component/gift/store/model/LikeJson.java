package com.trtc.uikit.livekit.component.gift.store.model;

import com.google.gson.annotations.SerializedName;

public class LikeJson {

    @SerializedName("data")
    public Data   data;
    @SerializedName("platform")
    public String platform;
    @SerializedName("version")
    public String version;
    @SerializedName("businessID")
    public String businessID;

    public static class Data {
        @SerializedName("sender")
        public User sender;

        public static class User {
            @SerializedName("userId")
            public String userId;
            @SerializedName("avatarUrl")
            public String avatarUrl;
            @SerializedName("userName")
            public String userName;
            @SerializedName("level")
            public String level;

            @Override
            public String toString() {
                return "User{"
                        + "userId='" + userId + '\''
                        + ", avatarUrl='" + avatarUrl + '\''
                        + ", userName='" + userName + '\''
                        + ", level='" + level + '\''
                        + '}';
            }
        }

        @Override
        public String toString() {
            return "Data{"
                    + "sender=" + sender
                    + '}';
        }
    }

    @Override
    public String toString() {
        return "TUIGiftLikeJson{"
                + "data=" + data
                + ", platform=" + platform
                + ", version=" + version
                + ", businessID=" + businessID
                + '}';
    }
}
