package com.trtc.uikit.livekit.voiceroom.manager.api.impl;

import static com.tencent.cloud.tuikit.engine.common.TUICommonDefine.ExtensionType.LIVE_LIST_MANAGER;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
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
import com.tencent.trtc.TRTCCloud;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.voiceroom.manager.api.IVoiceRoom;

import java.util.List;

public class VoiceRoomImpl implements IVoiceRoom {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getVoiceRoomLogger("VoiceRoomImpl");

    private final TUIRoomEngine      mTUIRoomEngine;
    private final TRTCCloud          mTRTCCloud;
    private final TUILiveListManager mTUILiveListManager;

    public VoiceRoomImpl() {
        mTUIRoomEngine = TUIRoomEngine.sharedInstance();
        mTRTCCloud = mTUIRoomEngine.getTRTCCloud();
        mTUILiveListManager = (TUILiveListManager) mTUIRoomEngine.getExtension(LIVE_LIST_MANAGER);
    }

    @Override
    public void destroy() {

    }

    @Override
    public void addRoomEngineObserver(TUIRoomObserver observer) {
        LOGGER.info("addRoomEngineObserver:[observer:" + observer.hashCode() + "]");
        mTUIRoomEngine.addObserver(observer);
    }

    @Override
    public void removeRoomEngineObserver(TUIRoomObserver observer) {
        LOGGER.info("removeRoomEngineObserver:[observer:" + observer.hashCode() + "]");
        mTUIRoomEngine.removeObserver(observer);
    }

    @Override
    public void addLiveListManagerObserver(TUILiveListManager.Observer observer) {
        LOGGER.info(" addLiveListManagerObserver:[observer:" + observer.hashCode() + "]");
        mTUILiveListManager.addObserver(observer);
    }

    @Override
    public void removeLiveListManagerObserver(TUILiveListManager.Observer observer) {
        LOGGER.info(" removeLiveListManagerObserver:[observer:" + observer.hashCode() + "]");
        mTUILiveListManager.removeObserver(observer);
    }

    @Override
    public void addFriendListener(V2TIMFriendshipListener listener) {
        LOGGER.info(" addFriendListener:[listener:" + listener.hashCode() + "]");
        V2TIMManager.getFriendshipManager().addFriendListener(listener);
    }

    @Override
    public void removeFriendListener(V2TIMFriendshipListener listener) {
        LOGGER.info(hashCode() + " removeFriendListener:[listener:" + listener.hashCode() + "]");
        V2TIMManager.getFriendshipManager().removeFriendListener(listener);
    }

    @Override
    public void getLiveInfo(String roomId, TUILiveListManager.LiveInfoCallback callback) {
        mTUILiveListManager.getLiveInfo(roomId, new TUILiveListManager.LiveInfoCallback() {
            @Override
            public void onSuccess(LiveInfo liveInfo) {
                LOGGER.info("getLiveInfo :[onSuccess:[liveInfo"
                        + new Gson().toJson(liveInfo) + "]]");
                if (callback != null) {
                    callback.onSuccess(liveInfo);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("getLiveInfo:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    /****************************************** Seat Business *******************************************/
    @Override
    public void getSeatList(TUIRoomDefine.GetSeatListCallback callback) {
        LOGGER.info("getSeatList:[]");
        mTUIRoomEngine.getSeatList(new TUIRoomDefine.GetSeatListCallback() {

            @Override
            public void onSuccess(List<TUIRoomDefine.SeatInfo> list) {
                LOGGER.info("getSeatList:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess(list);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("getSeatList:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    /****************************************** User Business *******************************************/
    @Override
    public void getUserList(long nextSequence, TUIRoomDefine.GetUserListCallback callback) {
        LOGGER.info("getUserList:[nextSequence:" + nextSequence + "]");
        mTUIRoomEngine.getUserList(nextSequence, new TUIRoomDefine.GetUserListCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserListResult userListResult) {
                LOGGER.info("getUserList:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess(userListResult);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("getUserList:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void getUserInfo(String userId, TUIRoomDefine.GetUserInfoCallback callback) {
        LOGGER.info("getUserInfo:[userId:" + userId + "]");
        mTUIRoomEngine.getUserInfo(userId, new TUIRoomDefine.GetUserInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserInfo userInfo) {
                LOGGER.info("getUserInfo:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess(userInfo);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("getUserInfo:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void muteAllRemoteAudio(boolean isMute) {
        LOGGER.info("muteAllRemoteAudio:[isMute:" + isMute + "]");
        mTRTCCloud.muteAllRemoteAudio(isMute);
    }

    /****************************************** IM Business *******************************************/
    @Override
    public void followUser(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowOperationResult>> callback) {
        LOGGER.info("followUser:[userIDList:" + userIDList + "]");
        V2TIMManager.getFriendshipManager().followUser(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowOperationResult>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowOperationResult> results) {
                        LOGGER.info("followUser:[onSuccess:[results:" + new Gson().toJson(results) + "]]");
                        if (callback != null) {
                            callback.onSuccess(results);
                        }
                    }

                    @Override
                    public void onError(int code, String message) {
                        LOGGER.error("followUser:[onError:[code:" + code + ",message:" + message + "]]");
                        if (callback != null) {
                            callback.onError(code, message);
                        }
                    }
                });
    }

    @Override
    public void unfollowUser(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowOperationResult>> callback) {
        LOGGER.info("unfollowUser:[userIDList:" + userIDList + "]");
        V2TIMManager.getFriendshipManager().unfollowUser(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowOperationResult>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowOperationResult> results) {
                        LOGGER.info("unfollowUser:[onSuccess:[results:" + new Gson().toJson(results)
                                + "]]");
                        if (callback != null) {
                            callback.onSuccess(results);
                        }
                    }

                    @Override
                    public void onError(int code, String message) {
                        LOGGER.error("unfollowUser:[onError:[code:" + code + ",message:" + message
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
        LOGGER.info("checkFollowType:[userIDList:" + userIDList + "]");
        V2TIMManager.getFriendshipManager().checkFollowType(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowTypeCheckResult>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowTypeCheckResult> results) {
                        LOGGER.info("checkFollowType:[onSuccess:[results:" + new Gson().toJson(results)
                                + "]]");
                        if (callback != null) {
                            callback.onSuccess(results);
                        }
                    }

                    @Override
                    public void onError(int code, String message) {
                        LOGGER.error("checkFollowType:[onSuccess:[code:" + code
                                + ",message:" + message + "]]");
                        if (callback != null) {
                            callback.onError(code, message);
                        }
                    }
                });
    }

    @Override
    public void setLiveInfo(LiveInfo liveInfo, List<LiveModifyFlag> flagList, TUIRoomDefine.ActionCallback callback) {
        LOGGER.info("setLiveInfo:[liveInfo:" + new Gson().toJson(liveInfo) + ",flag:" + flagList + "]");
        mTUILiveListManager.setLiveInfo(liveInfo, flagList, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LOGGER.info("setLiveInfo:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("setLiveInfo:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }
}
