package com.trtc.uikit.livekit.voiceroomcore.manager.api.impl;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.tencent.cloud.tuikit.engine.room.internal.TUIRoomEngineImpl;
import com.trtc.uikit.livekit.voiceroomcore.common.utils.Logger;
import com.trtc.uikit.livekit.voiceroomcore.manager.api.IVoiceRoomService;

import java.util.List;

public class VoiceRoomServiceImpl implements IVoiceRoomService {
    private static final String FILE = "VoiceRoomServiceImpl";

    private final TUIRoomEngine mTUIRoomEngine;

    public VoiceRoomServiceImpl() {
        mTUIRoomEngine = TUIRoomEngine.sharedInstance();
    }

    @Override
    public void addRoomEngineObserver(TUIRoomObserver observer) {
        Logger.info(FILE, "addRoomEngineObserver:[observer:" + observer.hashCode() + "]");
        mTUIRoomEngine.addObserver(observer);
    }

    @Override
    public void removeRoomEngineObserver(TUIRoomObserver observer) {
        Logger.info(FILE, "removeRoomEngineObserver:[observer:" + observer.hashCode() + "]");
        mTUIRoomEngine.removeObserver(observer);
    }

    /****************************************** Room Business *******************************************/
    @Override
    public void createRoom(TUIRoomDefine.RoomInfo roomInfo, TUIRoomDefine.ActionCallback callback) {
        Logger.info(FILE, "createRoom:[roomInfo:" + new Gson().toJson(roomInfo) + "]");
        mTUIRoomEngine.createRoom(roomInfo, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(FILE, "createRoom:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "createRoom:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }


    @Override
    public void enterRoom(String roomId, TUIRoomDefine.GetRoomInfoCallback callback) {
        mTUIRoomEngine.enterRoom(roomId, TUIRoomDefine.RoomType.LIVE, new TUIRoomDefine.GetRoomInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                Logger.info(FILE, "enterRoom:[onSuccess:[roomInfo" + new Gson().toJson(roomInfo) + "]]");
                if (callback != null) {
                    callback.onSuccess(roomInfo);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "enterRoom:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void exitRoom(TUIRoomDefine.ActionCallback callback) {
        Logger.info(FILE, "exitRoom:[isSyncWaiting" + true + "]");
        mTUIRoomEngine.exitRoom(true, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(FILE, "exitRoom:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "exitRoom:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void destroyRoom(TUIRoomDefine.ActionCallback callback) {
        Logger.info(FILE, "destroyRoom:[]");
        mTUIRoomEngine.destroyRoom(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(FILE, "destroyRoom:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "destroyRoom:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void updateRoomSeatMode(TUIRoomDefine.SeatMode seatMode, TUIRoomDefine.ActionCallback callback) {
        Logger.info(FILE, "updateRoomSeatMode: " + seatMode);
        mTUIRoomEngine.updateRoomSeatModeByAdmin(seatMode, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(FILE, "updateRoomSeatMode:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "updateRoomSeatMode:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    /****************************************** Seat Business *******************************************/
    @Override
    public TUIRoomDefine.Request takeSeat(int seatIndex, int timeout, TUIRoomDefine.RequestCallback callback) {
        Logger.info(FILE, "takeSeat:[seatIndex:" + seatIndex + ",timeout:" + timeout + "]");
        return mTUIRoomEngine.takeSeat(seatIndex, timeout, new TUIRoomDefine.RequestCallback() {
            @Override
            public void onAccepted(String requestId, String userId) {
                Logger.info(FILE, "takeSeat:[onAccepted:[requestId:" + requestId + ",userId:" + userId + "]]");
                if (callback != null) {
                    callback.onAccepted(requestId, userId);
                }
            }

            @Override
            public void onRejected(String requestId, String userId, String message) {
                Logger.info(FILE, "takeSeat:[onRejected:[requestId:" + requestId + ",userId:" + userId + "]]");
                if (callback != null) {
                    callback.onRejected(requestId, userId, message);
                }
            }

            @Override
            public void onCancelled(String requestId, String userId) {
                Logger.info(FILE, "takeSeat:[onCancelled:[requestId:" + requestId + ",userId:" + userId + "]]");
                if (callback != null) {
                    callback.onCancelled(requestId, userId);
                }
            }

            @Override
            public void onTimeout(String requestId, String userId) {
                Logger.info(FILE, "takeSeat:[onTimeout:[requestId:" + requestId + ",userId:" + userId + "]]");
                if (callback != null) {
                    callback.onTimeout(requestId, userId);
                }
            }

            @Override
            public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "takeSeat:onError, [requestId:" + requestId + ",userId:" + userId + ",error:"
                        + error + ",message:" + message + "]");
                if (callback != null) {
                    callback.onError(requestId, userId, error, message);
                }
            }
        });
    }

    @Override
    public void moveToSeat(int seatIndex, TUIRoomDefine.ActionCallback callback) {
        Logger.info(FILE, "moveToSeat:[seatIndex:" + seatIndex + "]");
        mTUIRoomEngine.moveToSeat(seatIndex, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(FILE, "moveToSeat:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "updateRoomSeatMode:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    public TUIRoomDefine.Request takeUserOnSeatByAdmin(int seatIndex, String userId, int timeout,
                                                       TUIRoomDefine.RequestCallback callback) {
        Logger.info(FILE, "takeUserOnSeatByAdmin:[seatIndex:" + seatIndex + ",userId:" + userId + ",timeout:"
                + timeout + "]");
        return mTUIRoomEngine.takeUserOnSeatByAdmin(seatIndex, userId, timeout, new TUIRoomDefine.RequestCallback() {
            @Override
            public void onAccepted(String requestId, String userId) {
                Logger.info(FILE, " takeUserOnSeatByAdmin:[onAccepted:[requestId:" + requestId + ",userId:" + userId
                        + "]]");
                if (callback != null) {
                    callback.onAccepted(requestId, userId);
                }
            }

            @Override
            public void onRejected(String requestId, String userId, String message) {
                Logger.info(FILE, "takeUserOnSeatByAdmin:[onRejected:[requestId:" + requestId + ",userId:" + userId
                        + "]]");
                if (callback != null) {
                    callback.onRejected(requestId, userId, message);
                }
            }

            @Override
            public void onCancelled(String requestId, String userId) {
                Logger.info(FILE, "takeUserOnSeatByAdmin:[onCancelled:[requestId:" + requestId + ",userId:" + userId
                        + "]]");
                if (callback != null) {
                    callback.onCancelled(requestId, userId);
                }
            }

            @Override
            public void onTimeout(String requestId, String userId) {
                Logger.info(FILE, "takeUserOnSeatByAdmin:[onTimeout:[requestId:" + requestId + ",userId:" + userId
                        + "]]");
                if (callback != null) {
                    callback.onTimeout(requestId, userId);
                }
            }

            @Override
            public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "takeUserOnSeatByAdmin:onError, [requestId:" + requestId + ",userId:" + userId
                        + ",error:" + error + ",message:" + message + "]");
                if (callback != null) {
                    callback.onError(requestId, userId, error, message);
                }
            }
        });
    }

    @Override
    public void leaveSeat(TUIRoomDefine.ActionCallback callback) {
        Logger.info(FILE, "leaveSeat:[]");
        mTUIRoomEngine.leaveSeat(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(FILE, "leaveSeat:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "leaveSeat:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void lockSeat(int seatIndex, TUIRoomDefine.SeatLockParams param, TUIRoomDefine.ActionCallback callback) {
        Logger.info(FILE, "lockSeatByAdmin:[seatIndex:" + seatIndex + "params:" + new Gson().toJson(param) + "]");
        mTUIRoomEngine.lockSeatByAdmin(seatIndex, param, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(FILE, "lockSeatByAdmin:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "lockSeatByAdmin:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void kickSeat(String userId, TUIRoomDefine.ActionCallback callback) {
        Logger.info(FILE, "kickSeat:[userId:" + userId + "]");
        mTUIRoomEngine.kickUserOffSeatByAdmin(-1, userId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(FILE, "kickUserOffSeatByAdmin:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "kickUserOffSeatByAdmin:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void getSeatList(TUIRoomDefine.GetSeatListCallback callback) {
        Logger.info(FILE, "getSeatList:[]");
        mTUIRoomEngine.getSeatList(new TUIRoomDefine.GetSeatListCallback() {

            @Override
            public void onSuccess(List<TUIRoomDefine.SeatInfo> list) {
                Logger.info(FILE, "getSeatList:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess(list);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "getSeatList:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void getSeatApplicationList(TUIRoomDefine.RequestListCallback callback) {
        Logger.info(FILE, "getSeatApplicationList:[]");
        mTUIRoomEngine.getSeatApplicationList(new TUIRoomDefine.RequestListCallback() {
            @Override
            public void onSuccess(List<TUIRoomDefine.Request> list) {
                Logger.info(FILE, "getSeatApplicationList:[onSuccess:[list:" + new Gson().toJson(list) + "]]");
                if (callback != null) {
                    callback.onSuccess(list);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "getSeatApplicationList:[onError:[error:" + error + ",message:" + message
                        + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void acceptRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        Logger.info(FILE, "responseRemoteRequest:[requestId:" + requestId + ",agree:" + true + "]");
        mTUIRoomEngine.responseRemoteRequest(requestId, true, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(FILE, "responseRemoteRequest:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "responseRemoteRequest:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void rejectRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        Logger.info(FILE, "responseRemoteRequest:[requestId:" + requestId + ",agree:" + false + "]");
        mTUIRoomEngine.responseRemoteRequest(requestId, false, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(FILE, "responseRemoteRequest:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "responseRemoteRequest:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void cancelRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        Logger.info(FILE, "cancelRequest:[requestId:" + requestId + "]");
        mTUIRoomEngine.cancelRequest(requestId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(FILE, "cancelRequest:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "cancelRequest:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void kickUserOffSeatByAdmin(String userId, TUIRoomDefine.ActionCallback callback) {
        Logger.info(FILE, "kickUserOffSeatByAdmin:[userId:" + userId + "]");
        mTUIRoomEngine.kickUserOffSeatByAdmin(0, userId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(FILE, "kickUserOffSeatByAdmin:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "kickUserOffSeatByAdmin:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    /****************************************** User Business *******************************************/
    @Override
    public void getUserInfo(String userId, TUIRoomDefine.GetUserInfoCallback callback) {
        Logger.info(FILE, "getUserInfo:[userId:" + userId + "]");
        mTUIRoomEngine.getUserInfo(userId, new TUIRoomDefine.GetUserInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserInfo userInfo) {
                Logger.info(FILE, "getUserInfo:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess(userInfo);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "getUserInfo:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    /****************************************** Media Business *******************************************/
    @Override
    public void openLocalMicrophone(TUIRoomDefine.ActionCallback callback) {
        Logger.info(FILE, "openLocalMicrophone:[]");
        mTUIRoomEngine.openLocalMicrophone(TUIRoomDefine.AudioQuality.DEFAULT, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(FILE, "openLocalMicrophone:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "openLocalMicrophone:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void closeLocalMicrophone() {
        Logger.info(FILE, "closeLocalMicrophone:[]");
        mTUIRoomEngine.closeLocalMicrophone();
    }

    @Override
    public void muteLocalAudio() {
        Logger.info(FILE, "muteLocalAudio:[]");
        mTUIRoomEngine.muteLocalAudio();
    }

    @Override
    public void unMuteLocalAudio(TUIRoomDefine.ActionCallback callback) {
        Logger.info(FILE, "unMuteLocalAudio:[]");
        mTUIRoomEngine.unmuteLocalAudio(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(FILE, "unMuteLocalAudio:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "unMuteLocalAudio:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void callExperimentalAPI(String jsonStr, Object param) {
        Logger.info(FILE, "callExperimentalAPI:[jsonStr:" + jsonStr + "param:" + new Gson().toJson(param) + "]");
        TUIRoomEngineImpl.callExperimentalAPI(jsonStr, param);
    }
}
