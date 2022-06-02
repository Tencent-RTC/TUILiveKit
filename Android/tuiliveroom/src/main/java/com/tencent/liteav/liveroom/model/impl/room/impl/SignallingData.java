package com.tencent.liteav.liveroom.model.impl.room.impl;

public class SignallingData {

    private int version;
    private String businessID;
    private String platform;
    private String extInfo;
    private DataInfo data;

    public int getVersion() {
        return version;
    }

    public void setVersion(int version) {
        this.version = version;
    }

    public String getBusinessID() {
        return businessID;
    }

    public void setBusinessID(String businessID) {
        this.businessID = businessID;
    }

    public String getPlatform() {
        return platform;
    }

    public void setPlatform(String platform) {
        this.platform = platform;
    }

    public String getExtInfo() {
        return extInfo;
    }

    public void setExtInfo(String extInfo) {
        this.extInfo = extInfo;
    }

    public DataInfo getData() {
        return data;
    }

    public void setData(DataInfo data) {
        this.data = data;
    }

    public static class DataInfo {
        private String roomId;
        private String cmd;

        public String getRoomID() {
            return roomId;
        }

        public void setRoomID(String roomID) {
            this.roomId = roomID;
        }

        public String getCmd() {
            return cmd;
        }

        public void setCmd(String cmd) {
            this.cmd = cmd;
        }
    }
}
