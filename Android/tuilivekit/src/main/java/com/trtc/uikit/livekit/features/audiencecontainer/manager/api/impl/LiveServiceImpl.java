package com.trtc.uikit.livekit.features.audiencecontainer.manager.api.impl;

import static com.tencent.cloud.tuikit.engine.common.TUICommonDefine.ExtensionType.LIVE_LAYOUT_MANAGER;
import static com.tencent.cloud.tuikit.engine.common.TUICommonDefine.ExtensionType.LIVE_LIST_MANAGER;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.cloud.tuikit.engine.extension.TUILiveLayoutManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveModifyFlag;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.tencent.imsdk.v2.V2TIMFollowInfo;
import com.tencent.imsdk.v2.V2TIMFollowOperationResult;
import com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult;
import com.tencent.imsdk.v2.V2TIMFriendshipListener;
import com.tencent.imsdk.v2.V2TIMManager;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.trtc.TRTCCloud;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.api.ILiveService;

import java.util.List;

public class LiveServiceImpl implements ILiveService {
    private static final LiveKitLogger        LOGGER = LiveKitLogger.getLiveStreamLogger("LiveServiceImpl");
    private final        TUIRoomEngine        mTUIRoomEngine;
    private final        TRTCCloud            mTRTCCloud;
    private final        TUILiveListManager   mTUILiveListManager;
    private final        TUILiveLayoutManager mTUILiveLayoutManager;


    public LiveServiceImpl() {
        mTUIRoomEngine = TUIRoomEngine.sharedInstance();
        mTRTCCloud = mTUIRoomEngine.getTRTCCloud();
        mTUILiveListManager = (TUILiveListManager) mTUIRoomEngine.getExtension(LIVE_LIST_MANAGER);
        mTUILiveLayoutManager = (TUILiveLayoutManager) mTUIRoomEngine.getExtension(LIVE_LAYOUT_MANAGER);
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
    public void addLiveLayoutManagerObserver(TUILiveLayoutManager.Observer observer) {
        LOGGER.info(hashCode() + " addLiveLayoutManagerObserver:[observer:" + observer.hashCode() + "]");
        mTUILiveLayoutManager.addObserver(observer);
    }

    @Override
    public void removeLiveLayoutManagerObserver(TUILiveLayoutManager.Observer observer) {
        LOGGER.info(hashCode() + " removeLiveLayoutManagerObserver:[observer:" + observer.hashCode() + "]");
        mTUILiveLayoutManager.removeObserver(observer);
    }

    /****************************************** Room Business *******************************************/
    @Override
    public void start(TUIRoomDefine.RoomInfo roomInfo, TUIRoomDefine.GetRoomInfoCallback callback) {
        createRoom(roomInfo, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                enterRoom(roomInfo.roomId, callback);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void join(String roomId, TUIRoomDefine.GetRoomInfoCallback callback) {
        enterRoom(roomId, callback);
    }

    @Override
    public void leave(TUIRoomDefine.ActionCallback callback) {
        LOGGER.info(hashCode() + " exitRoom:[isSyncWaiting" + true + "]");
        mTUIRoomEngine.exitRoom(true, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LOGGER.info(hashCode() + " exitRoom:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error(hashCode() + " exitRoom:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void stop(TUIRoomDefine.ActionCallback callback) {
        LOGGER.info(hashCode() + " destroyRoom:[]");
        mTUIRoomEngine.destroyRoom(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LOGGER.info(hashCode() + " destroyRoom:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error(hashCode() + " destroyRoom:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    private void createRoom(TUIRoomDefine.RoomInfo roomInfo, TUIRoomDefine.ActionCallback callback) {
        loginRoomEngine(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LOGGER.info(hashCode() + " createRoom:[roomInfo:" + new Gson().toJson(roomInfo) + "]");
                mTUIRoomEngine.createRoom(roomInfo, callback);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {
            }
        });
    }

    private void enterRoom(String roomId, TUIRoomDefine.GetRoomInfoCallback callback) {
        loginRoomEngine(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LOGGER.info(hashCode() + " enterRoom:[roomId:" + roomId + ", roomType:" + TUIRoomDefine.RoomType.LIVE + "]");
                mTUIRoomEngine.enterRoom(roomId, TUIRoomDefine.RoomType.LIVE, new TUIRoomDefine.GetRoomInfoCallback() {
                    @Override
                    public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                        LOGGER.info(hashCode() + " enterRoom:[onSuccess:[roomInfo" + new Gson().toJson(roomInfo) +
                                "]]");
                        if (callback != null) {
                            callback.onSuccess(roomInfo);
                        }
                    }

                    @Override
                    public void onError(TUICommonDefine.Error error, String message) {
                        LOGGER.error(hashCode() + " enterRoom:[onError:[error:" + error + ",message:" + message +
                                "]]");
                        if (callback != null) {
                            callback.onError(error, message);
                        }
                    }
                });
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {
            }
        });
    }

    private void loginRoomEngine(TUIRoomDefine.ActionCallback callback) {
        TUIRoomEngine.login(TUILogin.getAppContext(), TUILogin.getSdkAppId(), TUILogin.getUserId(),
                TUILogin.getUserSig(), callback);
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

    public void updateRoomSeatModeByAdmin(TUIRoomDefine.SeatMode seatMode, TUIRoomDefine.ActionCallback callback) {
        LOGGER.info(hashCode() + " updateRoomSeatModeByAdmin, seatMode:" + seatMode + "]");
        mTUIRoomEngine.updateRoomSeatModeByAdmin(seatMode, callback);
    }

    /****************************************** Seat Business *******************************************/
    @Override
    public TUIRoomDefine.Request takeSeat(int seatIndex, int timeout, TUIRoomDefine.RequestCallback callback) {
        LOGGER.info(hashCode() + " takeSeat:[seatIndex:" + seatIndex + ",timeout:" + timeout + "]");
        return mTUIRoomEngine.takeSeat(seatIndex, timeout, new TUIRoomDefine.RequestCallback() {
            @Override
            public void onAccepted(String requestId, String userId) {
                LOGGER.info(hashCode() + " takeSeat:[onAccepted:[requestId:" + requestId + ",userId:" + userId +
                        "]]");
                if (callback != null) {
                    callback.onAccepted(requestId, userId);
                }
            }

            @Override
            public void onRejected(String requestId, String userId, String message) {
                LOGGER.info(hashCode() + " takeSeat:[onRejected:[requestId:" + requestId + ",userId:" + userId +
                        "]]");
                if (callback != null) {
                    callback.onRejected(requestId, userId, message);
                }
            }

            @Override
            public void onCancelled(String requestId, String userId) {
                LOGGER.info(hashCode() + " takeSeat:[onCancelled:[requestId:" + requestId + ",userId:" + userId +
                        "]]");
                if (callback != null) {
                    callback.onCancelled(requestId, userId);
                }
            }

            @Override
            public void onTimeout(String requestId, String userId) {
                LOGGER.info(hashCode() + " takeSeat:[onTimeout:[requestId:" + requestId + ",userId:" + userId + "]]");
                if (callback != null) {
                    callback.onTimeout(requestId, userId);
                }
            }

            @Override
            public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {
                LOGGER.error(hashCode() + " takeSeat:onError, [requestId:" + requestId + ",userId:" + userId + "," +
                        "error:" + error + ",message:" + message + "]");
                if (callback != null) {
                    callback.onError(requestId, userId, error, message);
                }
            }
        });
    }

    public TUIRoomDefine.Request takeUserOnSeatByAdmin(int seatIndex, String userId, int timeout,
                                                       TUIRoomDefine.RequestCallback callback) {
        LOGGER.info(hashCode() + " takeUserOnSeatByAdmin:[seatIndex:" + seatIndex + ",userId:" + userId + ",timeout" +
                ":" + timeout + "]");
        return mTUIRoomEngine.takeUserOnSeatByAdmin(seatIndex, userId, timeout, new TUIRoomDefine.RequestCallback() {
            @Override
            public void onAccepted(String requestId, String userId) {
                LOGGER.info(hashCode() + " takeUserOnSeatByAdmin:[onAccepted:[requestId:" + requestId + ",userId:" + userId + "]]");
                if (callback != null) {
                    callback.onAccepted(requestId, userId);
                }
            }

            @Override
            public void onRejected(String requestId, String userId, String message) {
                LOGGER.info(hashCode() + " takeUserOnSeatByAdmin:[onRejected:[requestId:" + requestId + ",userId:" + userId + "]]");
                if (callback != null) {
                    callback.onRejected(requestId, userId, message);
                }
            }

            @Override
            public void onCancelled(String requestId, String userId) {
                LOGGER.info(hashCode() + " takeUserOnSeatByAdmin:[onCancelled:[requestId:" + requestId + ",userId:" + userId + "]]");
                if (callback != null) {
                    callback.onCancelled(requestId, userId);
                }
            }

            @Override
            public void onTimeout(String requestId, String userId) {
                LOGGER.info(hashCode() + " takeUserOnSeatByAdmin:[onTimeout:[requestId:" + requestId + ",userId:" + userId + "]]");
                if (callback != null) {
                    callback.onTimeout(requestId, userId);
                }
            }

            @Override
            public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {
                LOGGER.error(hashCode() + " takeUserOnSeatByAdmin:onError, [requestId:" + requestId + ",userId:" + userId + ",error:" + error + ",message:" + message + "]");
                if (callback != null) {
                    callback.onError(requestId, userId, error, message);
                }
            }
        });
    }

    @Override
    public void leaveSeat(TUIRoomDefine.ActionCallback callback) {
        LOGGER.info(hashCode() + " leaveSeat:[]");
        mTUIRoomEngine.leaveSeat(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LOGGER.info(hashCode() + " leaveSeat:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error(hashCode() + " leaveSeat:[onError:[error:" + error + ",message:" + message + "]]");
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
    public void kickSeat(String userId, TUIRoomDefine.ActionCallback callback) {
        LOGGER.info(hashCode() + " kickSeat:[userId:" + userId + "]");
        mTUIRoomEngine.kickUserOffSeatByAdmin(-1, userId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LOGGER.info(hashCode() + " kickUserOffSeatByAdmin:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error(hashCode() + " kickUserOffSeatByAdmin:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void getSeatList(TUIRoomDefine.GetSeatListCallback callback) {
        LOGGER.info(hashCode() + " getSeatList:[]");
        mTUIRoomEngine.getSeatList(new TUIRoomDefine.GetSeatListCallback() {

            @Override
            public void onSuccess(List<TUIRoomDefine.SeatInfo> list) {
                LOGGER.info(hashCode() + " getSeatList:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess(list);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error(hashCode() + " getSeatList:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void getSeatApplicationList(TUIRoomDefine.RequestListCallback callback) {
        LOGGER.info(hashCode() + " getSeatApplicationList:[]");
        mTUIRoomEngine.getSeatApplicationList(new TUIRoomDefine.RequestListCallback() {
            @Override
            public void onSuccess(List<TUIRoomDefine.Request> list) {
                LOGGER.info(hashCode() + " getSeatApplicationList:[onSuccess:[list:" + new Gson().toJson(list) +
                        "]]");
                if (callback != null) {
                    callback.onSuccess(list);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error(hashCode() + " getSeatApplicationList:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void acceptRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        LOGGER.info(hashCode() + " responseRemoteRequest:[requestId:" + requestId + ",agree:" + true + "]");
        mTUIRoomEngine.responseRemoteRequest(requestId, true, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LOGGER.info(hashCode() + " responseRemoteRequest:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error(hashCode() + " responseRemoteRequest:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void rejectRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        LOGGER.info(hashCode() + " responseRemoteRequest:[requestId:" + requestId + ",agree:" + false + "]");
        mTUIRoomEngine.responseRemoteRequest(requestId, false, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LOGGER.info(hashCode() + " responseRemoteRequest:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error(hashCode() + " responseRemoteRequest:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void cancelRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        LOGGER.info(hashCode() + " cancelRequest:[requestId:" + requestId + "]");
        mTUIRoomEngine.cancelRequest(requestId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LOGGER.info(hashCode() + " cancelRequest:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error(hashCode() + " cancelRequest:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void kickUserOffSeatByAdmin(int seatIndex, String userId, TUIRoomDefine.ActionCallback callback) {
        LOGGER.info(hashCode() + " kickUserOffSeatByAdmin:[seatIndex:" + seatIndex + "userId:" + userId + "]");
        mTUIRoomEngine.kickUserOffSeatByAdmin(seatIndex, userId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LOGGER.info(hashCode() + " kickUserOffSeatByAdmin:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error(hashCode() + " kickUserOffSeatByAdmin:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    /****************************************** User Business *******************************************/
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
    public void muteAllRemoteAudio(boolean isMute) {
        LOGGER.info(hashCode() + " muteAllRemoteAudio:[isMute:" + isMute + "]");
        mTRTCCloud.muteAllRemoteAudio(isMute);
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

    /****************************************** Media Business *******************************************/
    @Override
    public void setLocalVideoView(TUIVideoView videoView) {
        LOGGER.info(hashCode() + " setLocalVideoView:[videoView:" + videoView + "]");
        mTUIRoomEngine.setLocalVideoView(videoView);
    }

    @Override
    public void setRemoteVideoView(String userId, TUIRoomDefine.VideoStreamType streamType, TUIVideoView videoView) {
        LOGGER.info(hashCode() + " setRemoteVideoView:[userId:" + userId + "streamType:" + streamType + "videoView" +
                ":" + videoView + "]");
        mTUIRoomEngine.setRemoteVideoView(userId, streamType, videoView);
    }

    @Override
    public void startPlayRemoteVideo(String userId, TUIRoomDefine.VideoStreamType streamType,
                                     TUIRoomDefine.PlayCallback callback) {
        LOGGER.info(hashCode() + " startPlayRemoteVideo:[userId:" + userId + "streamType:" + streamType + "]");
        mTUIRoomEngine.startPlayRemoteVideo(userId, streamType, new TUIRoomDefine.PlayCallback() {
            @Override
            public void onPlaying(String userId) {
                LOGGER.info(hashCode() + " startPlayRemoteVideo:[onPlaying:[userId" + userId + "]]");
                if (callback != null) {
                    callback.onPlaying(userId);
                }
            }

            @Override
            public void onLoading(String userId) {
                LOGGER.info(hashCode() + " startPlayRemoteVideo:[onLoading:[userId" + userId + "]]");
                if (callback != null) {
                    callback.onLoading(userId);
                }
            }

            @Override
            public void onPlayError(String userId, TUICommonDefine.Error error, String message) {
                LOGGER.info(hashCode() + " startPlayRemoteVideo:[onPlayError:[userId" + userId + ",error" + error +
                        ",message" + message + "]]");
                if (callback != null) {
                    callback.onPlayError(userId, error, message);
                }
            }
        });
    }

    @Override
    public void muteLocalAudio() {
        LOGGER.info(hashCode() + " muteLocalAudio:[]");
        mTUIRoomEngine.muteLocalAudio();
    }

    @Override
    public void unMuteLocalAudio(TUIRoomDefine.ActionCallback callback) {
        LOGGER.info(hashCode() + " unMuteLocalAudio:[]");
        mTUIRoomEngine.unmuteLocalAudio(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LOGGER.info(hashCode() + " unMuteLocalAudio:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error(hashCode() + " unMuteLocalAudio:[onError:[error:" + error + ",message:" + message +
                        "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void enableGravitySensor(boolean enable) {
        LOGGER.info(hashCode() + " enableGravitySensor:[enable:" + enable + "]");
        mTUIRoomEngine.enableGravitySensor(enable);
    }

    @Override
    public void updateAudioQuality(TUIRoomDefine.AudioQuality quality) {
        LOGGER.info(hashCode() + " updateAudioQuality:[quality:" + quality + "]");
        mTUIRoomEngine.updateAudioQuality(quality);
    }

    @Override
    public void setVideoResolutionMode(TUIRoomDefine.ResolutionMode resolutionMode) {
        LOGGER.info(hashCode() + " setVideoResolutionMode:[resolutionMode:" + resolutionMode + "]");
        mTUIRoomEngine.setVideoResolutionMode(TUIRoomDefine.VideoStreamType.CAMERA_STREAM, resolutionMode);
    }

    /****************************************** IM Business *******************************************/
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
    public void getUserFollowInfo(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowInfo>> callback) {
        LOGGER.info(hashCode() + " getUserFollowInfo:[userIDList:" + userIDList + "]");
        V2TIMManager.getFriendshipManager().getUserFollowInfo(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowInfo>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowInfo> results) {
                        LOGGER.info(hashCode() + " getUserFollowInfo:[onSuccess:[results:" + new Gson().toJson(results) +
                                "]]");
                        if (callback != null) {
                            callback.onSuccess(results);
                        }
                    }

                    @Override
                    public void onError(int code, String message) {
                        LOGGER.error(hashCode() + " getUserFollowInfo:[onSuccess:[code:" + code + ",message:" + message +
                                "]]");
                        if (callback != null) {
                            callback.onError(code, message);
                        }
                    }
                });
    }

    @Override
    public void callExperimentalAPI(String jsonStr) {
        LOGGER.info(hashCode() + " callExperimentalAPI:[jsonStr:" + jsonStr + "]");
        TUIRoomEngine.sharedInstance().callExperimentalAPI(jsonStr, null);
    }

    /****************************************** Plugin - Room List *******************************************/
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

    @Override
    public TRTCCloud getTRTCCloud() {
        return mTRTCCloud;
    }
}
