package com.tencent.qcloud.tuikit.tuigift.model;

import com.google.gson.annotations.SerializedName;

/**
 * IM发送的礼物信息封装类
 */
public class TUIGiftJson {

    @SerializedName("data")
    private Data   data;
    @SerializedName("platform")
    private String platform;
    @SerializedName("version")
    private String version;
    @SerializedName("businessID")
    private String businessID;

    public Data getData() {
        return data;
    }

    public void setData(Data data) {
        this.data = data;
    }

    public String getPlatform() {
        return platform;
    }

    public void setPlatform(String platform) {
        this.platform = platform;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public String getBusinessID() {
        return businessID;
    }

    public void setBusinessID(String businessID) {
        this.businessID = businessID;
    }

    public static class Data {
        @SerializedName("extInfo")
        private ExtInfo extInfo;
        @SerializedName("giftId")
        public  String  giftId;
        @SerializedName("imageUrl")
        public  String  imageUrl;
        @SerializedName("lottieUrl")
        public  String  lottieUrl;
        @SerializedName("message")
        public  String  message;

        public ExtInfo getExtInfo() {
            return extInfo;
        }

        public void setExtInfo(ExtInfo extInfo) {
            this.extInfo = extInfo;
        }

        public String getGiftId() {
            return giftId;
        }

        public void setGiftId(String giftId) {
            this.giftId = giftId;
        }

        public String getImageUrl() {
            return imageUrl;
        }

        public void setImageUrl(String imageUrl) {
            this.imageUrl = imageUrl;
        }

        public String getLottieUrl() {
            return lottieUrl;
        }

        public void setLottieUrl(String lottieUrl) {
            this.lottieUrl = lottieUrl;
        }

        public String getMessage() {
            return message;
        }

        public void setMessage(String message) {
            this.message = message;
        }

        public static class ExtInfo {
            @SerializedName("userID")
            private String userID;
            @SerializedName("avatarUrl")
            private String avatarUrl;
            @SerializedName("nickName")
            private String nickName;

            public String getUserID() {
                return userID;
            }

            public void setUserID(String userID) {
                this.userID = userID;
            }

            public String getAvatarUrl() {
                return avatarUrl;
            }

            public void setAvatarUrl(String avatarUrl) {
                this.avatarUrl = avatarUrl;
            }

            public String getNickName() {
                return nickName;
            }

            public void setNickName(String nickName) {
                this.nickName = nickName;
            }

            @Override
            public String toString() {
                return "ExtInfo{"
                        + "userID=" + userID
                        + ", avatarUrl=" + avatarUrl
                        + ", nickName=" + nickName
                        + '}';
            }
        }

        @Override
        public String toString() {
            return "Data{"
                    + "extInfo=" + extInfo
                    + ", giftId=" + giftId
                    + ", imageUrl=" + imageUrl
                    + ", lottieUrl=" + lottieUrl
                    + ", message=" + message
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
