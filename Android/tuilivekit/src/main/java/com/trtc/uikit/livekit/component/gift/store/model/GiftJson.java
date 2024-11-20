package com.trtc.uikit.livekit.component.gift.store.model;

import com.google.gson.annotations.SerializedName;

import java.util.HashMap;
import java.util.Map;

public class GiftJson {

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
        @SerializedName("receiver")
        public User receiver;
        @SerializedName("gift")
        public Gift gift;
        @SerializedName("giftCount")
        public int  giftCount;

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

        public static class Gift {
            @SerializedName("giftId")
            public  String  giftId;
            @SerializedName("giftName")
            public  String  giftName;
            @SerializedName("imageUrl")
            public  String  imageUrl;
            @SerializedName("animationUrl")
            public  String  animationUrl;
            @SerializedName("price")
            public int       price;
            @SerializedName("extInfo")
            public Map<String, String> extInfo = new HashMap<>();

            @Override
            public String toString() {
                return "Gift{"
                        + "giftId=" + giftId
                        + ", giftName=" + giftName
                        + ", imageUrl=" + imageUrl
                        + ", animationUrl=" + animationUrl
                        + ", price=" + price
                        + ", extInfo=" + extInfo
                        + '}';
            }
        }

        @Override
        public String toString() {
            return "Data{"
                    + "sender=" + sender
                    + ", receiver=" + receiver
                    + ", gift=" + gift
                    + ", giftCount=" + giftCount
                    + '}';
        }
    }

    @Override
    public String toString() {
        return "TUIGiftJson{"
                + "data=" + data
                + ", platform=" + platform
                + ", version=" + version
                + ", businessID=" + businessID
                + '}';
    }
}
