package com.tencent.liteav.liveroom.model;


import java.util.List;

public class LiveRoomManager {

    private static LiveRoomManager sInstance;
    private RoomCallback mRoomCallback;

    public static LiveRoomManager getInstance() {
        if (sInstance == null) {
            synchronized (LiveRoomManager.class) {
                if (sInstance == null) {
                    sInstance = new LiveRoomManager();
                }
            }
        }
        return sInstance;
    }


    public void addCallback(RoomCallback callback) {
        mRoomCallback = callback;
    }

    public void removeCallback() {
        mRoomCallback = null;
    }

    public void createRoom(int roomId, ActionCallback callback) {
        if (mRoomCallback != null) {
            mRoomCallback.onRoomCreate(roomId, callback);
        }
    }

    public void destroyRoom(int roomId, ActionCallback callback) {
        if (mRoomCallback != null) {
            mRoomCallback.onRoomDestroy(roomId, callback);
        }
    }

    public void getRoomIdList(GetCallback callback) {
        if (mRoomCallback != null) {
            mRoomCallback.onGetRoomIdList(callback);
        }
    }

    public interface RoomCallback {
        void onRoomCreate(int roomId, ActionCallback callback);
        void onRoomDestroy(int roomId, ActionCallback callback);
        void onGetRoomIdList(GetCallback callback);
    }

    public interface ActionCallback {
        void onSuccess();
        void onError(int code, String message);
    }

    public interface GetCallback {
        void onSuccess(List<Integer> list);
        void onError(int code, String message);
    }
}
