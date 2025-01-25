package com.trtc.uikit.livekit.livestream.manager.api.impl;

import static com.tencent.cloud.tuikit.engine.common.TUICommonDefine.ExtensionType.LIVE_LIST_MANAGER;
import static com.tencent.trtc.TRTCCloudDef.TRTC_VIDEO_MIRROR_TYPE_DISABLE;
import static com.tencent.trtc.TRTCCloudDef.TRTC_VIDEO_MIRROR_TYPE_ENABLE;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
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
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.trtc.TRTCCloud;
import com.tencent.trtc.TRTCCloudDef;
import com.trtc.uikit.livekit.livestream.manager.api.ILiveService;
import com.trtc.uikit.livekit.livestream.manager.api.LiveStreamLog;
import com.trtc.uikit.livekit.livestream.manager.error.ErrorHandler;

import java.util.List;

public class LiveServiceImpl implements ILiveService {
    private final String               mTag = "LiveServiceImpl[" + hashCode() + "]";
    private final TUIRoomEngine        mTUIRoomEngine;
    private final TRTCCloud            mTRTCCloud;
    private final TUILiveListManager   mTUILiveListManager;

    public LiveServiceImpl() {
        mTUIRoomEngine = TUIRoomEngine.sharedInstance();
        mTRTCCloud = mTUIRoomEngine.getTRTCCloud();
        mTUILiveListManager = (TUILiveListManager) mTUIRoomEngine.getExtension(LIVE_LIST_MANAGER);
    }

    @Override
    public void destroy() {
        LiveStreamLog.info(mTag + " destroy");
    }

    @Override
    public void addRoomEngineObserver(TUIRoomObserver observer) {
        LiveStreamLog.info(mTag + " addRoomEngineObserver:[observer:" + observer.hashCode() + "]");
        mTUIRoomEngine.addObserver(observer);
    }

    @Override
    public void removeRoomEngineObserver(TUIRoomObserver observer) {
        LiveStreamLog.info(mTag + " removeRoomEngineObserver:[observer:" + observer.hashCode() + "]");
        mTUIRoomEngine.removeObserver(observer);
    }

    @Override
    public void addLiveListManagerObserver(TUILiveListManager.Observer observer) {
        LiveStreamLog.info(mTag + " addLiveListManagerObserver:[observer:" + observer.hashCode() + "]");
        mTUILiveListManager.addObserver(observer);
    }

    @Override
    public void removeLiveListManagerObserver(TUILiveListManager.Observer observer) {
        LiveStreamLog.info(mTag + " removeLiveListManagerObserver:[observer:" + observer.hashCode() + "]");
        mTUILiveListManager.removeObserver(observer);
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
        LiveStreamLog.info(mTag + " exitRoom:[isSyncWaiting" + true + "]");
        mTUIRoomEngine.exitRoom(true, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveStreamLog.info(mTag + " exitRoom:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(mTag + " exitRoom:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void stop(TUIRoomDefine.ActionCallback callback) {
        LiveStreamLog.info(mTag + " destroyRoom:[]");
        mTUIRoomEngine.destroyRoom(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveStreamLog.info(mTag + " destroyRoom:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(mTag + " destroyRoom:[onError:[error:" + error + ",message:" + message + "]]");
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
                LiveStreamLog.info(mTag + " createRoom:[roomInfo:" + new Gson().toJson(roomInfo) + "]");
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
                LiveStreamLog.info(mTag
                        + " enterRoom:[roomId:" + roomId + ", roomType:" + TUIRoomDefine.RoomType.LIVE + "]");
                mTUIRoomEngine.enterRoom(roomId, TUIRoomDefine.RoomType.LIVE, new TUIRoomDefine.GetRoomInfoCallback() {
                    @Override
                    public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                        LiveStreamLog.info(mTag + " enterRoom:[onSuccess:[roomInfo" + new Gson().toJson(roomInfo) +
                                "]]");
                        if (callback != null) {
                            callback.onSuccess(roomInfo);
                        }
                    }

                    @Override
                    public void onError(TUICommonDefine.Error error, String message) {
                        LiveStreamLog.error(mTag + " enterRoom:[onError:[error:" + error + ",message:" + message +
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
                LiveStreamLog.info(mTag + " getLiveInfo :[onSuccess:[liveInfo"
                        + new Gson().toJson(liveInfo) + "]]");
                if (callback != null) {
                    callback.onSuccess(liveInfo);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(mTag + " getLiveInfo:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    public void updateRoomSeatModeByAdmin(TUIRoomDefine.SeatMode seatMode, TUIRoomDefine.ActionCallback callback) {
        LiveStreamLog.info(mTag + " updateRoomSeatModeByAdmin, seatMode:" + seatMode + "]");
        mTUIRoomEngine.updateRoomSeatModeByAdmin(seatMode, callback);
    }

    /****************************************** Seat Business *******************************************/
    @Override
    public TUIRoomDefine.Request takeSeat(int seatIndex, int timeout, TUIRoomDefine.RequestCallback callback) {
        LiveStreamLog.info(mTag + " takeSeat:[seatIndex:" + seatIndex + ",timeout:" + timeout + "]");
        return mTUIRoomEngine.takeSeat(seatIndex, timeout, new TUIRoomDefine.RequestCallback() {
            @Override
            public void onAccepted(String requestId, String userId) {
                LiveStreamLog.info(mTag + " takeSeat:[onAccepted:[requestId:" + requestId + ",userId:" + userId + "]]");
                if (callback != null) {
                    callback.onAccepted(requestId, userId);
                }
            }

            @Override
            public void onRejected(String requestId, String userId, String message) {
                LiveStreamLog.info(mTag + " takeSeat:[onRejected:[requestId:" + requestId + ",userId:" + userId + "]]");
                if (callback != null) {
                    callback.onRejected(requestId, userId, message);
                }
            }

            @Override
            public void onCancelled(String requestId, String userId) {
                LiveStreamLog.info(mTag + " takeSeat:[onCancelled:[requestId:" + requestId + ",userId:" + userId +
                        "]]");
                if (callback != null) {
                    callback.onCancelled(requestId, userId);
                }
            }

            @Override
            public void onTimeout(String requestId, String userId) {
                LiveStreamLog.info(mTag + " takeSeat:[onTimeout:[requestId:" + requestId + ",userId:" + userId + "]]");
                if (callback != null) {
                    callback.onTimeout(requestId, userId);
                }
            }

            @Override
            public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(mTag + " takeSeat:onError, [requestId:" + requestId + ",userId:" + userId + "," +
                        "error:"
                        + error + ",message:" + message + "]");
                if (callback != null) {
                    callback.onError(requestId, userId, error, message);
                }
            }
        });
    }

    public TUIRoomDefine.Request takeUserOnSeatByAdmin(int seatIndex, String userId, int timeout,
                                                       TUIRoomDefine.RequestCallback callback) {
        LiveStreamLog.info(mTag + " takeUserOnSeatByAdmin:[seatIndex:" + seatIndex + ",userId:" + userId + ",timeout:"
                + timeout + "]");
        return mTUIRoomEngine.takeUserOnSeatByAdmin(seatIndex, userId, timeout, new TUIRoomDefine.RequestCallback() {
            @Override
            public void onAccepted(String requestId, String userId) {
                LiveStreamLog.info(mTag + " takeUserOnSeatByAdmin:[onAccepted:[requestId:" + requestId + ",userId:"
                        + userId + "]]");
                if (callback != null) {
                    callback.onAccepted(requestId, userId);
                }
            }

            @Override
            public void onRejected(String requestId, String userId, String message) {
                LiveStreamLog.info(mTag + " takeUserOnSeatByAdmin:[onRejected:[requestId:" + requestId + ",userId:"
                        + userId + "]]");
                if (callback != null) {
                    callback.onRejected(requestId, userId, message);
                }
            }

            @Override
            public void onCancelled(String requestId, String userId) {
                LiveStreamLog.info(mTag + " takeUserOnSeatByAdmin:[onCancelled:[requestId:" + requestId + ",userId:"
                        + userId + "]]");
                if (callback != null) {
                    callback.onCancelled(requestId, userId);
                }
            }

            @Override
            public void onTimeout(String requestId, String userId) {
                LiveStreamLog.info(mTag + " takeUserOnSeatByAdmin:[onTimeout:[requestId:" + requestId + ",userId:"
                        + userId + "]]");
                if (callback != null) {
                    callback.onTimeout(requestId, userId);
                }
            }

            @Override
            public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(mTag + " takeUserOnSeatByAdmin:onError, [requestId:" + requestId + ",userId:" + userId
                        + ",error:" + error + ",message:" + message + "]");
                if (callback != null) {
                    callback.onError(requestId, userId, error, message);
                }
            }
        });
    }

    @Override
    public void leaveSeat(TUIRoomDefine.ActionCallback callback) {
        LiveStreamLog.info(mTag + " leaveSeat:[]");
        mTUIRoomEngine.leaveSeat(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveStreamLog.info(mTag + " leaveSeat:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(mTag + " leaveSeat:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void lockSeat(int seatIndex, TUIRoomDefine.SeatLockParams param, TUIRoomDefine.ActionCallback callback) {
        LiveStreamLog.info(mTag + " lockSeatByAdmin:[seatIndex:" + seatIndex + "params:" + new Gson().toJson(param) + "]");
        mTUIRoomEngine.lockSeatByAdmin(seatIndex, param, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveStreamLog.info(mTag + " lockSeatByAdmin:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(mTag + " lockSeatByAdmin:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void kickSeat(String userId, TUIRoomDefine.ActionCallback callback) {
        LiveStreamLog.info(mTag + " kickSeat:[userId:" + userId + "]");
        mTUIRoomEngine.kickUserOffSeatByAdmin(-1, userId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveStreamLog.info(mTag + " kickUserOffSeatByAdmin:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(mTag + " kickUserOffSeatByAdmin:[onError:[error:" + error + ",message:" + message
                        + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void getSeatList(TUIRoomDefine.GetSeatListCallback callback) {
        LiveStreamLog.info(mTag + " getSeatList:[]");
        mTUIRoomEngine.getSeatList(new TUIRoomDefine.GetSeatListCallback() {

            @Override
            public void onSuccess(List<TUIRoomDefine.SeatInfo> list) {
                LiveStreamLog.info(mTag + " getSeatList:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess(list);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(mTag + " getSeatList:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void getSeatApplicationList(TUIRoomDefine.RequestListCallback callback) {
        LiveStreamLog.info(mTag + " getSeatApplicationList:[]");
        mTUIRoomEngine.getSeatApplicationList(new TUIRoomDefine.RequestListCallback() {
            @Override
            public void onSuccess(List<TUIRoomDefine.Request> list) {
                LiveStreamLog.info(mTag + " getSeatApplicationList:[onSuccess:[list:" + new Gson().toJson(list) + "]]");
                if (callback != null) {
                    callback.onSuccess(list);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(mTag + " getSeatApplicationList:[onError:[error:" + error + ",message:" + message
                        + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void acceptRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        LiveStreamLog.info(mTag + " responseRemoteRequest:[requestId:" + requestId + ",agree:" + true + "]");
        mTUIRoomEngine.responseRemoteRequest(requestId, true, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveStreamLog.info(mTag + " responseRemoteRequest:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(mTag + " responseRemoteRequest:[onError:[error:" + error + ",message:" + message
                        + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void rejectRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        LiveStreamLog.info(mTag + " responseRemoteRequest:[requestId:" + requestId + ",agree:" + false + "]");
        mTUIRoomEngine.responseRemoteRequest(requestId, false, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveStreamLog.info(mTag + " responseRemoteRequest:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(mTag + " responseRemoteRequest:[onError:[error:" + error + ",message:" + message
                        + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void cancelRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        LiveStreamLog.info(mTag + " cancelRequest:[requestId:" + requestId + "]");
        mTUIRoomEngine.cancelRequest(requestId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveStreamLog.info(mTag + " cancelRequest:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(mTag + " cancelRequest:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void kickUserOffSeatByAdmin(int seatIndex, String userId, TUIRoomDefine.ActionCallback callback) {
        LiveStreamLog.info(mTag + " kickUserOffSeatByAdmin:[seatIndex:" + seatIndex + "userId:" + userId + "]");
        mTUIRoomEngine.kickUserOffSeatByAdmin(seatIndex, userId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveStreamLog.info(mTag + " kickUserOffSeatByAdmin:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(mTag + " kickUserOffSeatByAdmin:[onError:[error:" + error + ",message:" + message
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
        LiveStreamLog.info(mTag + " getUserList:[nextSequence:" + nextSequence + "]");
        mTUIRoomEngine.getUserList(nextSequence, new TUIRoomDefine.GetUserListCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserListResult userListResult) {
                LiveStreamLog.info(mTag + " getUserList:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess(userListResult);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(mTag + " getUserList:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void getUserInfo(String userId, TUIRoomDefine.GetUserInfoCallback callback) {
        LiveStreamLog.info(mTag + " getUserInfo:[userId:" + userId + "]");
        mTUIRoomEngine.getUserInfo(userId, new TUIRoomDefine.GetUserInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserInfo userInfo) {
                LiveStreamLog.info(mTag + " getUserInfo:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess(userInfo);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(mTag + " getUserInfo:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void muteAllRemoteAudio(boolean isMute) {
        LiveStreamLog.info(mTag + " muteAllRemoteAudio:[isMute:" + isMute + "]");
        mTRTCCloud.muteAllRemoteAudio(isMute);
    }

    /****************************************** Media Business *******************************************/
    @Override
    public void openLocalMicrophone(TUIRoomDefine.ActionCallback callback) {
        LiveStreamLog.info(mTag + " openLocalMicrophone:[]");
        mTUIRoomEngine.openLocalMicrophone(TUIRoomDefine.AudioQuality.DEFAULT, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveStreamLog.info(mTag + " openLocalMicrophone:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(mTag + " openLocalMicrophone:[onError:[error:" + error + ",message:" + message +
                        "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void openLocalCamera(boolean isFront, TUIRoomDefine.VideoQuality quality,
                                TUIRoomDefine.ActionCallback callback) {
        LiveStreamLog.info(mTag + " openLocalCamera:[isFront:" + isFront + "quality:" + quality + "]");
        mTUIRoomEngine.openLocalCamera(isFront, quality, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveStreamLog.info(mTag + " openLocalCamera:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(mTag + " openLocalCamera:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void closeLocalCamera() {
        LiveStreamLog.info(mTag + " closeLocalCamera:[]");
        mTUIRoomEngine.closeLocalCamera();
    }

    @Override
    public void setCameraMirror(boolean isMirror) {
        LiveStreamLog.info(mTag + " setVideoEncoderMirror:[isMirror:" + isMirror + "]");
        TRTCCloudDef.TRTCRenderParams trtcRenderParams = new TRTCCloudDef.TRTCRenderParams();
        trtcRenderParams.mirrorType = isMirror ? TRTC_VIDEO_MIRROR_TYPE_ENABLE : TRTC_VIDEO_MIRROR_TYPE_DISABLE;
        LiveStreamLog.info(mTag + " setLocalRenderParams:[trtcRenderParams:" + new Gson().toJson(trtcRenderParams) +
                "]");
        mTRTCCloud.setLocalRenderParams(trtcRenderParams);
        mTRTCCloud.setVideoEncoderMirror(isMirror);
    }

    @Override
    public void switchCamera(boolean isFrontCamera) {
        LiveStreamLog.info(mTag + " switchCamera:[isFrontCamera:" + isFrontCamera + "]");
        mTUIRoomEngine.switchCamera(isFrontCamera);
    }

    @Override
    public void setLocalVideoView(TUIVideoView videoView) {
        LiveStreamLog.info(mTag + " setLocalVideoView:[videoView:" + videoView + "]");
        mTUIRoomEngine.setLocalVideoView(videoView);
    }

    @Override
    public void setRemoteVideoView(String userId, TUIRoomDefine.VideoStreamType streamType, TUIVideoView videoView) {
        LiveStreamLog.info(mTag + " setRemoteVideoView:[userId:" + userId + "streamType:" + streamType
                + "videoView:" + videoView + "]");
        mTUIRoomEngine.setRemoteVideoView(userId, streamType, videoView);
    }

    @Override
    public void startPlayRemoteVideo(String userId, TUIRoomDefine.VideoStreamType streamType,
                                     TUIRoomDefine.PlayCallback callback) {
        LiveStreamLog.info(mTag + " startPlayRemoteVideo:[userId:" + userId + "streamType:" + streamType + "]");
        mTUIRoomEngine.startPlayRemoteVideo(userId, streamType, new TUIRoomDefine.PlayCallback() {
            @Override
            public void onPlaying(String userId) {
                LiveStreamLog.info(mTag + " startPlayRemoteVideo:[onPlaying:[userId" + userId + "]]");
                if (callback != null) {
                    callback.onPlaying(userId);
                }
            }

            @Override
            public void onLoading(String userId) {
                LiveStreamLog.info(mTag + " startPlayRemoteVideo:[onLoading:[userId" + userId + "]]");
                if (callback != null) {
                    callback.onLoading(userId);
                }
            }

            @Override
            public void onPlayError(String userId, TUICommonDefine.Error error, String message) {
                LiveStreamLog.info(mTag + " startPlayRemoteVideo:[onPlayError:[userId" + userId + ",error" + error
                        + ",message" + message + "]]");
                if (callback != null) {
                    callback.onPlayError(userId, error, message);
                }
            }
        });
    }

    @Override
    public void muteLocalAudio() {
        LiveStreamLog.info(mTag + " muteLocalAudio:[]");
        mTUIRoomEngine.muteLocalAudio();
    }

    @Override
    public void unMuteLocalAudio(TUIRoomDefine.ActionCallback callback) {
        LiveStreamLog.info(mTag + " unMuteLocalAudio:[]");
        mTUIRoomEngine.unmuteLocalAudio(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveStreamLog.info(mTag + " unMuteLocalAudio:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(mTag + " unMuteLocalAudio:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void enableGravitySensor(boolean enable) {
        LiveStreamLog.info(mTag + " enableGravitySensor:[enable:" + enable + "]");
        mTUIRoomEngine.enableGravitySensor(enable);
    }

    @Override
    public void updateVideoQuality(TUIRoomDefine.VideoQuality quality) {
        LiveStreamLog.info(mTag + " updateVideoQuality:[quality:" + quality + "]");
        mTUIRoomEngine.updateVideoQuality(quality);
    }

    @Override
    public void updateAudioQuality(TUIRoomDefine.AudioQuality quality) {
        LiveStreamLog.info(mTag + " updateAudioQuality:[quality:" + quality + "]");
        mTUIRoomEngine.updateAudioQuality(quality);
    }

    @Override
    public void setVideoResolutionMode(TUIRoomDefine.ResolutionMode resolutionMode) {
        LiveStreamLog.info(mTag + " setVideoResolutionMode:[resolutionMode:" + resolutionMode + "]");
        mTUIRoomEngine.setVideoResolutionMode(TUIRoomDefine.VideoStreamType.CAMERA_STREAM, resolutionMode);
    }

    @Override
    public void setBeautyStyle(int style) {
        LiveStreamLog.info(mTag + " setBeautyStyle:[style:" + style + "]");
        mTRTCCloud.getBeautyManager().setBeautyStyle(style);
    }

    @Override
    public void setBeautyLevel(float level) {
        LiveStreamLog.info(mTag + " setBeautyLevel:[level:" + level + "]");
        mTRTCCloud.getBeautyManager().setBeautyLevel(level);
    }

    @Override
    public void setWhitenessLevel(float level) {
        LiveStreamLog.info(mTag + " setWhitenessLevel:[level:" + level + "]");
        mTRTCCloud.getBeautyManager().setWhitenessLevel(level);
    }

    @Override
    public void setRuddyLevel(float level) {
        LiveStreamLog.info(mTag + " setRuddyLevel:[level:" + level + "]");
        mTRTCCloud.getBeautyManager().setRuddyLevel(level);
    }

    /****************************************** IM Business *******************************************/
    @Override
    public void followUser(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowOperationResult>> callback) {
        LiveStreamLog.info(mTag + " followUser:[userIDList:" + userIDList + "]");
        V2TIMManager.getFriendshipManager().followUser(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowOperationResult>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowOperationResult> results) {
                        LiveStreamLog.info(mTag + " followUser:[onSuccess:[results:" + new Gson().toJson(results) +
                                "]]");
                        if (callback != null) {
                            callback.onSuccess(results);
                        }
                    }

                    @Override
                    public void onError(int code, String message) {
                        LiveStreamLog.error(mTag + " followUser:[onSuccess:[code:" + code + ",message:" + message +
                                "]]");
                        if (callback != null) {
                            callback.onError(code, message);
                        }
                    }
                });
    }

    @Override
    public void unfollowUser(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowOperationResult>> callback) {
        LiveStreamLog.info(mTag + " unfollowUser:[userIDList:" + userIDList + "]");
        V2TIMManager.getFriendshipManager().unfollowUser(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowOperationResult>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowOperationResult> results) {
                        LiveStreamLog.info(mTag + " unfollowUser:[onSuccess:[results:" + new Gson().toJson(results)
                                + "]]");
                        if (callback != null) {
                            callback.onSuccess(results);
                        }
                    }

                    @Override
                    public void onError(int code, String message) {
                        LiveStreamLog.error(mTag + " unfollowUser:[onSuccess:[code:" + code + ",message:" + message
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
        LiveStreamLog.info(mTag + " checkFollowType:[userIDList:" + userIDList + "]");
        V2TIMManager.getFriendshipManager().checkFollowType(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowTypeCheckResult>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowTypeCheckResult> results) {
                        LiveStreamLog.info(mTag + " checkFollowType:[onSuccess:[results:" + new Gson().toJson(results)
                                + "]]");
                        if (callback != null) {
                            callback.onSuccess(results);
                        }
                    }

                    @Override
                    public void onError(int code, String message) {
                        LiveStreamLog.error(mTag + " checkFollowType:[onSuccess:[code:" + code
                                + ",message:" + message + "]]");
                        if (callback != null) {
                            callback.onError(code, message);
                        }
                    }
                });
    }

    @Override
    public void getUserFollowInfo(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowInfo>> callback) {
        LiveStreamLog.info(mTag + " getUserFollowInfo:[userIDList:" + userIDList + "]");
        V2TIMManager.getFriendshipManager().getUserFollowInfo(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowInfo>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowInfo> results) {
                        LiveStreamLog.info(mTag + " getUserFollowInfo:[onSuccess:[results:" + new Gson().toJson(results)
                                + "]]");
                        if (callback != null) {
                            callback.onSuccess(results);
                        }
                    }

                    @Override
                    public void onError(int code, String message) {
                        LiveStreamLog.error(mTag + " getUserFollowInfo:[onSuccess:[code:" + code
                                + ",message:" + message + "]]");
                        if (callback != null) {
                            callback.onError(code, message);
                        }
                    }
                });
    }

    @Override
    public void callExperimentalAPI(String jsonStr) {
        LiveStreamLog.info(mTag + " callExperimentalAPI:[jsonStr:" + jsonStr + "]");
        TUIRoomEngine.callExperimentalAPI(jsonStr);
    }

    /****************************************** Plugin - Room List *******************************************/
    @Override
    public void fetchLiveList(String cursor, int count, TUILiveListManager.LiveInfoListCallback callback) {
        LiveStreamLog.info(mTag + " fetchLiveList:[cursor:" + cursor + ",count:" + count + "]");
        mTUILiveListManager.fetchLiveList(cursor, count, new TUILiveListManager.LiveInfoListCallback() {
            @Override
            public void onSuccess(TUILiveListManager.LiveInfoListResult liveInfoListResult) {
                LiveStreamLog.info(mTag + " fetchLiveList:[onSuccess:[liveInfoListResult:"
                        + new Gson().toJson(liveInfoListResult));
                if (callback != null) {
                    callback.onSuccess(liveInfoListResult);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {
                LiveStreamLog.error(mTag + " fetchLiveList:[onError:[error:" + error + ",s:" + s + "]]");
                if (ErrorHandler.interceptErrorCode(error, s)) {
                    return;
                }
                if (callback != null) {
                    callback.onError(error, s);
                }
            }
        });
    }

    @Override
    public void setLiveInfo(LiveInfo liveInfo, List<LiveModifyFlag> flagList, TUIRoomDefine.ActionCallback callback) {
        LiveStreamLog.info((mTag + " setLiveInfo:[liveInfo:" + new Gson().toJson(liveInfo) + ",flag:" + flagList +
                "]"));
        mTUILiveListManager.setLiveInfo(liveInfo, flagList, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveStreamLog.info(mTag + " setLiveInfo:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(mTag + " setLiveInfo:[onError:[error:" + error + ",message:" + message + "]]");
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
