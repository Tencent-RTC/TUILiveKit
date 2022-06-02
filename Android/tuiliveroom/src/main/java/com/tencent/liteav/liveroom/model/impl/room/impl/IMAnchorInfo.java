package com.tencent.liteav.liveroom.model.impl.room.impl;

public class IMAnchorInfo {
    public String userId;
    public String streamId;
    public String name;

    public IMAnchorInfo() {
        clean();
    }

    public void clean() {
        userId = "";
        streamId = "";
        name = "";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        IMAnchorInfo info = (IMAnchorInfo) o;
        return info.userId != null && this.userId != null && info.userId.equals(this.userId);
    }

    @Override
    public String toString() {
        return "IMAnchorInfo{"
                + "userId='" + userId + '\''
                + ", streamId='" + streamId + '\''
                + ", name='" + name + '\''
                + '}';
    }
}
