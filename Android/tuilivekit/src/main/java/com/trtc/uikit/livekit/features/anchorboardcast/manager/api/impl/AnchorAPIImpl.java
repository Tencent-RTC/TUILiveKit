package com.trtc.uikit.livekit.features.anchorboardcast.manager.api.impl;

import static com.tencent.cloud.tuikit.engine.common.TUICommonDefine.ExtensionType.LIVE_LIST_MANAGER;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveModifyFlag;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.tencent.imsdk.v2.V2TIMFollowOperationResult;
import com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult;
import com.tencent.imsdk.v2.V2TIMFriendshipListener;
import com.tencent.imsdk.v2.V2TIMManager;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.api.IAnchorAPI;

import java.util.List;

public class AnchorAPIImpl implements IAnchorAPI {
    private static final LiveKitLogger      LOGGER = LiveKitLogger.getFeaturesLogger("AnchorAPIImpl");
    private final        TUIRoomEngine      mTUIRoomEngine;
    private final        TUILiveListManager mTUILiveListManager;

    public AnchorAPIImpl() {
        mTUIRoomEngine = TUIRoomEngine.sharedInstance();
        mTUILiveListManager = (TUILiveListManager) mTUIRoomEngine.getExtension(LIVE_LIST_MANAGER);
    }

    @Override
    public void destroy() {
        LOGGER.info(hashCode() + " destroy");
    }

    @Override
    public void addRoomEngineObserver(TUIRoomObserver observer) {
        LOGGER.info(hashCode() + " addRoomEngineObserver:[observer:" + observer.hashCode() + "]");
        mTUIRoomEngine.addObserver(observer);
    }

    @Override
    public void removeRoomEngineObserver(TUIRoomObserver observer) {
        LOGGER.info(hashCode() + " removeRoomEngineObserver:[observer:" + observer.hashCode() + "]");
        mTUIRoomEngine.removeObserver(observer);
    }

    @Override
    public void addLiveListManagerObserver(TUILiveListManager.Observer observer) {
        LOGGER.info(hashCode() + " addLiveListManagerObserver:[observer:" + observer.hashCode() + "]");
        mTUILiveListManager.addObserver(observer);
    }

    @Override
    public void removeLiveListManagerObserver(TUILiveListManager.Observer observer) {
        LOGGER.info(hashCode() + " removeLiveListManagerObserver:[observer:" + observer.hashCode() + "]");
        mTUILiveListManager.removeObserver(observer);
    }

    @Override
    public void addFriendListener(V2TIMFriendshipListener listener) {
        LOGGER.info(hashCode() + " addFriendListener:[listener:" + listener.hashCode() + "]");
        V2TIMManager.getFriendshipManager().addFriendListener(listener);
    }

    @Override
    public void removeFriendListener(V2TIMFriendshipListener listener) {
        LOGGER.info(hashCode() + " removeFriendListener:[observer:" + listener.hashCode() + "]");
        V2TIMManager.getFriendshipManager().removeFriendListener(listener);
    }

    @Override
    public void getLiveInfo(String roomId, TUILiveListManager.LiveInfoCallback callback) {
        mTUILiveListManager.getLiveInfo(roomId, new TUILiveListManager.LiveInfoCallback() {
            @Override
            public void onSuccess(LiveInfo liveInfo) {
                LOGGER.info(hashCode() + " getLiveInfo :[onSuccess:[liveInfo" + new Gson().toJson(liveInfo) + "]]");
                if (callback != null) {
                    callback.onSuccess(liveInfo);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error(hashCode() + " getLiveInfo:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void lockSeat(int seatIndex, TUIRoomDefine.SeatLockParams param, TUIRoomDefine.ActionCallback callback) {
        LOGGER.info(hashCode() + " lockSeatByAdmin:[seatIndex:" + seatIndex + "params:" + new Gson().toJson(param) +
                "]");
        mTUIRoomEngine.lockSeatByAdmin(seatIndex, param, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LOGGER.info(hashCode() + " lockSeatByAdmin:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error(hashCode() + " lockSeatByAdmin:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void getUserList(long nextSequence, TUIRoomDefine.GetUserListCallback callback) {
        LOGGER.info(hashCode() + " getUserList:[nextSequence:" + nextSequence + "]");
        mTUIRoomEngine.getUserList(nextSequence, new TUIRoomDefine.GetUserListCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserListResult userListResult) {
                LOGGER.info(hashCode() + " getUserList:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess(userListResult);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error(hashCode() + " getUserList:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void getUserInfo(String userId, TUIRoomDefine.GetUserInfoCallback callback) {
        LOGGER.info(hashCode() + " getUserInfo:[userId:" + userId + "]");
        mTUIRoomEngine.getUserInfo(userId, new TUIRoomDefine.GetUserInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserInfo userInfo) {
                LOGGER.info(hashCode() + " getUserInfo:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess(userInfo);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error(hashCode() + " getUserInfo:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void disableSendingMessageByAdmin(String userId, boolean isDisable, TUIRoomDefine.ActionCallback callback) {
        LOGGER.info(hashCode() + " disableSendingMessageByAdmin:[userId:" + userId + ",isDisable:" + isDisable + "]");
        mTUIRoomEngine.disableSendingMessageByAdmin(userId, isDisable, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LOGGER.info(hashCode() + " disableSendingMessageByAdmin:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error(hashCode() + " disableSendingMessageByAdmin:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void kickRemoteUserOutOfRoom(String userId, TUIRoomDefine.ActionCallback callback) {
        LOGGER.info(hashCode() + " kickRemoteUserOutOfRoom:[userId:" + userId + "]");
        mTUIRoomEngine.kickRemoteUserOutOfRoom(userId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LOGGER.info(hashCode() + " kickRemoteUserOutOfRoom:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error(hashCode() + " kickRemoteUserOutOfRoom:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void setLocalVideoView(TUIVideoView videoView) {
        LOGGER.info(hashCode() + " setLocalVideoView:[videoView:" + videoView + "]");
        mTUIRoomEngine.setLocalVideoView(videoView);
    }

    @Override
    public void followUser(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowOperationResult>> callback) {
        LOGGER.info(hashCode() + " followUser:[userIDList:" + userIDList + "]");
        V2TIMManager.getFriendshipManager().followUser(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowOperationResult>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowOperationResult> results) {
                        LOGGER.info(hashCode() + " followUser:[onSuccess:[results:" + new Gson().toJson(results) +
                                "]]");
                        if (callback != null) {
                            callback.onSuccess(results);
                        }
                    }

                    @Override
                    public void onError(int code, String message) {
                        LOGGER.error(hashCode() + " followUser:[onSuccess:[code:" + code + ",message:" + message +
                                "]]");
                        if (callback != null) {
                            callback.onError(code, message);
                        }
                    }
                });
    }

    @Override
    public void unfollowUser(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowOperationResult>> callback) {
        LOGGER.info(hashCode() + " unfollowUser:[userIDList:" + userIDList + "]");
        V2TIMManager.getFriendshipManager().unfollowUser(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowOperationResult>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowOperationResult> results) {
                        LOGGER.info(hashCode() + " unfollowUser:[onSuccess:[results:" + new Gson().toJson(results) +
                                "]]");
                        if (callback != null) {
                            callback.onSuccess(results);
                        }
                    }

                    @Override
                    public void onError(int code, String message) {
                        LOGGER.error(hashCode() + " unfollowUser:[onSuccess:[code:" + code + ",message:" + message +
                                "]]");
                        if (callback != null) {
                            callback.onError(code, message);
                        }
                    }
                });
    }

    @Override
    public void checkFollowType(List<String> userIDList,
                                V2TIMValueCallback<List<V2TIMFollowTypeCheckResult>> callback) {
        LOGGER.info(hashCode() + " checkFollowType:[userIDList:" + userIDList + "]");
        V2TIMManager.getFriendshipManager().checkFollowType(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowTypeCheckResult>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowTypeCheckResult> results) {
                        LOGGER.info(hashCode() + " checkFollowType:[onSuccess:[results:" + new Gson().toJson(results) + "]]");
                        if (callback != null) {
                            callback.onSuccess(results);
                        }
                    }

                    @Override
                    public void onError(int code, String message) {
                        LOGGER.error(hashCode() + " checkFollowType:[onSuccess:[code:" + code + ",message:" + message + "]]");
                        if (callback != null) {
                            callback.onError(code, message);
                        }
                    }
                });
    }

    @Override
    public void fetchLiveList(String cursor, int count, TUILiveListManager.LiveInfoListCallback callback) {
        LOGGER.info(hashCode() + " fetchLiveList:[cursor:" + cursor + ",count:" + count + "]");
        mTUILiveListManager.fetchLiveList(cursor, count, new TUILiveListManager.LiveInfoListCallback() {
            @Override
            public void onSuccess(TUILiveListManager.LiveInfoListResult liveInfoListResult) {
                LOGGER.info(hashCode() + " fetchLiveList:[onSuccess:[liveInfoListResult:" + new Gson().toJson(liveInfoListResult));
                if (callback != null) {
                    callback.onSuccess(liveInfoListResult);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {
                LOGGER.error(hashCode() + " fetchLiveList:[onError:[error:" + error + ",s:" + s + "]]");
                if (callback != null) {
                    callback.onError(error, s);
                }
            }
        });
    }

    @Override
    public void setLiveInfo(LiveInfo liveInfo, List<LiveModifyFlag> flagList, TUIRoomDefine.ActionCallback callback) {
        LOGGER.info((hashCode() + " setLiveInfo:[liveInfo:" + new Gson().toJson(liveInfo) + ",flag:" + flagList +
                "]"));
        mTUILiveListManager.setLiveInfo(liveInfo, flagList, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LOGGER.info(hashCode() + " setLiveInfo:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error(hashCode() + " setLiveInfo:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }
}
