package com.trtc.uikit.livekit.voiceroom.api.impl;

import static com.tencent.cloud.tuikit.engine.common.TUICommonDefine.ExtensionType.LIVE_LIST_MANAGER;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveModifyFlag;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.tencent.imsdk.v2.V2TIMFollowInfo;
import com.tencent.imsdk.v2.V2TIMFollowOperationResult;
import com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult;
import com.tencent.imsdk.v2.V2TIMManager;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.tencent.trtc.TRTCCloud;
import com.trtc.uikit.livekit.voiceroom.api.IVoiceRoom;
import com.trtc.uikit.livekit.voiceroom.api.Logger;

import java.util.List;

public class VoiceRoomImpl implements IVoiceRoom {
    private final String FILE = "VoiceRoomImpl";

    private final TUIRoomEngine            mTUIRoomEngine;
    private final TRTCCloud                mTRTCCloud;
    private final TUILiveListManager       mTUILiveListManager;
    private final TUILiveConnectionManager mTUILiveConnectionManager;

    public VoiceRoomImpl() {
        mTUIRoomEngine = TUIRoomEngine.sharedInstance();
        mTRTCCloud = mTUIRoomEngine.getTRTCCloud();
        mTUILiveListManager = (TUILiveListManager) mTUIRoomEngine.getExtension(LIVE_LIST_MANAGER);
        mTUILiveConnectionManager = mTUIRoomEngine.getLiveConnectionManager();
    }

    @Override
    public void destroy() {

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

    @Override
    public void addLiveListManagerObserver(TUILiveListManager.Observer observer) {
        Logger.info(FILE, " addLiveListManagerObserver:[observer:" + observer.hashCode() + "]");
        mTUILiveListManager.addObserver(observer);
    }

    @Override
    public void removeLiveListManagerObserver(TUILiveListManager.Observer observer) {
        Logger.info(FILE, " removeLiveListManagerObserver:[observer:" + observer.hashCode() + "]");
        mTUILiveListManager.removeObserver(observer);
    }

    @Override
    public void getLiveInfo(String roomId, TUILiveListManager.LiveInfoCallback callback) {
        mTUILiveListManager.getLiveInfo(roomId, new TUILiveListManager.LiveInfoCallback() {
            @Override
            public void onSuccess(LiveInfo liveInfo) {
                Logger.info(FILE, "getLiveInfo :[onSuccess:[liveInfo"
                        + new Gson().toJson(liveInfo) + "]]");
                if (callback != null) {
                    callback.onSuccess(liveInfo);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "getLiveInfo:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    /****************************************** Seat Business *******************************************/
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

    /****************************************** Seat Business *******************************************/
    @Override
    public TUIRoomDefine.Request takeSeat(int seatIndex, int timeout, TUIRoomDefine.RequestCallback callback) {
        Logger.info(FILE, " takeSeat:[seatIndex:" + seatIndex + ",timeout:" + timeout + "]");
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

    /****************************************** User Business *******************************************/
    @Override
    public void getUserList(long nextSequence, TUIRoomDefine.GetUserListCallback callback) {
        Logger.info(FILE, "getUserList:[nextSequence:" + nextSequence + "]");
        mTUIRoomEngine.getUserList(nextSequence, new TUIRoomDefine.GetUserListCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserListResult userListResult) {
                Logger.info(FILE, "getUserList:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess(userListResult);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "getUserList:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

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

    @Override
    public void muteAllRemoteAudio(boolean isMute) {
        Logger.info(FILE, "muteAllRemoteAudio:[isMute:" + isMute + "]");
        mTRTCCloud.muteAllRemoteAudio(isMute);
    }

    /****************************************** IM Business *******************************************/
    @Override
    public void followUser(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowOperationResult>> callback) {
        Logger.info(FILE, "followUser:[userIDList:" + userIDList + "]");
        V2TIMManager.getFriendshipManager().followUser(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowOperationResult>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowOperationResult> results) {
                        Logger.info(FILE, "followUser:[onSuccess:[results:" + new Gson().toJson(results) + "]]");
                        if (callback != null) {
                            callback.onSuccess(results);
                        }
                    }

                    @Override
                    public void onError(int code, String message) {
                        Logger.error(FILE, "followUser:[onSuccess:[code:" + code + ",message:" + message + "]]");
                        if (callback != null) {
                            callback.onError(code, message);
                        }
                    }
                });
    }

    @Override
    public void unfollowUser(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowOperationResult>> callback) {
        Logger.info(FILE, "unfollowUser:[userIDList:" + userIDList + "]");
        V2TIMManager.getFriendshipManager().unfollowUser(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowOperationResult>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowOperationResult> results) {
                        Logger.info(FILE, "unfollowUser:[onSuccess:[results:" + new Gson().toJson(results)
                                + "]]");
                        if (callback != null) {
                            callback.onSuccess(results);
                        }
                    }

                    @Override
                    public void onError(int code, String message) {
                        Logger.error(FILE, "unfollowUser:[onSuccess:[code:" + code + ",message:" + message
                                + "]]");
                        if (callback != null) {
                            callback.onError(code, message);
                        }
                    }
                });
    }

    @Override
    public void checkFollowType(List<String> userIDList,
                                V2TIMValueCallback<List<V2TIMFollowTypeCheckResult>> callback) {
        Logger.info(FILE, "checkFollowType:[userIDList:" + userIDList + "]");
        V2TIMManager.getFriendshipManager().checkFollowType(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowTypeCheckResult>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowTypeCheckResult> results) {
                        Logger.info(FILE, "checkFollowType:[onSuccess:[results:" + new Gson().toJson(results)
                                + "]]");
                        if (callback != null) {
                            callback.onSuccess(results);
                        }
                    }

                    @Override
                    public void onError(int code, String message) {
                        Logger.error(FILE, "checkFollowType:[onSuccess:[code:" + code
                                + ",message:" + message + "]]");
                        if (callback != null) {
                            callback.onError(code, message);
                        }
                    }
                });
    }

    @Override
    public void getUserFollowInfo(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowInfo>> callback) {
        Logger.info(FILE, "getUserFollowInfo:[userIDList:" + userIDList + "]");
        V2TIMManager.getFriendshipManager().getUserFollowInfo(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowInfo>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowInfo> results) {
                        Logger.info(FILE, "getUserFollowInfo:[onSuccess:[results:" + new Gson().toJson(results)
                                + "]]");
                        if (callback != null) {
                            callback.onSuccess(results);
                        }
                    }

                    @Override
                    public void onError(int code, String message) {
                        Logger.error(FILE, "getUserFollowInfo:[onSuccess:[code:" + code
                                + ",message:" + message + "]]");
                        if (callback != null) {
                            callback.onError(code, message);
                        }
                    }
                });
    }

    @Override
    public void setLiveInfo(LiveInfo liveInfo, List<LiveModifyFlag> flagList, TUIRoomDefine.ActionCallback callback) {
        Logger.info(FILE, "setLiveInfo:[liveInfo:" + new Gson().toJson(liveInfo) + ",flag:" + flagList + "]");
        mTUILiveListManager.setLiveInfo(liveInfo, flagList, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(FILE, "setLiveInfo:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "setLiveInfo:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }
}
