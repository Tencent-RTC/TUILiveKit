package com.trtc.uikit.livekit.service.impl;

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
import com.tencent.cloud.tuikit.engine.room.internal.TUIRoomEngineImpl;
import com.tencent.imsdk.v2.V2TIMFollowInfo;
import com.tencent.imsdk.v2.V2TIMFollowOperationResult;
import com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult;
import com.tencent.imsdk.v2.V2TIMManager;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.tencent.trtc.TRTCCloud;
import com.tencent.trtc.TRTCCloudDef;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.service.ILiveService;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.List;

public class LiveServiceImpl implements ILiveService {
    private final String mTag = "RoomEngineService[" + hashCode() + "]";

    private final TUIRoomEngine      mTUIRoomEngine;
    private final TRTCCloud          mTRTCCloud;
    private final TUILiveListManager mTUILiveListManager;

    public LiveServiceImpl() {
        mTUIRoomEngine = createEngine();
        mTRTCCloud = mTUIRoomEngine.getTRTCCloud();
        mTUILiveListManager = (TUILiveListManager) mTUIRoomEngine.getExtension(LIVE_LIST_MANAGER);
    }

    @Override
    public void destroy() {
        destroyEngine();
    }

    @Override
    public void addRoomEngineObserver(TUIRoomObserver observer) {
        LiveKitLog.info(mTag + " addRoomEngineObserver:[observer:" + observer + "]");
        mTUIRoomEngine.addObserver(observer);
    }

    @Override
    public void removeRoomEngineObserver(TUIRoomObserver observer) {
        LiveKitLog.info(mTag + " removeRoomEngineObserver:[observer:" + observer + "]");
        mTUIRoomEngine.removeObserver(observer);
    }

    @Override
    public void addLiveListManagerObserver(TUILiveListManager.Observer observer) {
        LiveKitLog.info(mTag + " addLiveListManagerObserver:[observer:" + observer + "]");
        mTUILiveListManager.addObserver(observer);
    }

    @Override
    public void removeLiveListManagerObserver(TUILiveListManager.Observer observer) {
        LiveKitLog.info(mTag + " removeLiveListManagerObserver:[observer:" + observer + "]");
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
        LiveKitLog.info(mTag + " exitRoom:[isSyncWaiting" + true + "]");
        mTUIRoomEngine.exitRoom(true, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(mTag + " exitRoom:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " exitRoom:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void stop(TUIRoomDefine.ActionCallback callback) {
        LiveKitLog.info(mTag + " destroyRoom:[]");
        mTUIRoomEngine.destroyRoom(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(mTag + " destroyRoom:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " destroyRoom:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    private void createRoom(TUIRoomDefine.RoomInfo roomInfo, TUIRoomDefine.ActionCallback callback) {
        LiveKitLog.info(mTag + " createRoom:[roomInfo:" + new Gson().toJson(roomInfo) + "]");
        mTUIRoomEngine.createRoom(roomInfo, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(mTag + " createRoom:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " createRoom:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    private void enterRoom(String roomId, TUIRoomDefine.GetRoomInfoCallback callback) {
        LiveKitLog.info(mTag + " enterRoom:[roomId:" + roomId + ", roomType:" + TUIRoomDefine.RoomType.LIVE + "]");
        mTUIRoomEngine.enterRoom(roomId, TUIRoomDefine.RoomType.LIVE, new TUIRoomDefine.GetRoomInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                LiveKitLog.info(mTag + " enterRoom:[onSuccess:[roomInfo" + new Gson().toJson(roomInfo) + "]]");
                if (callback != null) {
                    callback.onSuccess(roomInfo);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " enterRoom:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void getLiveInfo(String roomId, TUILiveListManager.LiveInfoCallback callback) {
        mTUILiveListManager.getLiveInfo(roomId, new TUILiveListManager.LiveInfoCallback() {
            @Override
            public void onSuccess(LiveInfo liveInfo) {
                LiveKitLog.info(mTag + " getLiveInfo :[onSuccess:[liveInfo"
                        + new Gson().toJson(liveInfo) + "]]");
                if (callback != null) {
                    callback.onSuccess(liveInfo);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " getLiveInfo:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    public void updateRoomSeatModeByAdmin(TUIRoomDefine.SeatMode seatMode, TUIRoomDefine.ActionCallback callback) {
        LiveKitLog.info(mTag + " updateRoomSeatModeByAdmin, seatMode:" + seatMode + "]");
        mTUIRoomEngine.updateRoomSeatModeByAdmin(seatMode, callback);
    }

    /****************************************** Seat Business *******************************************/
    @Override
    public TUIRoomDefine.Request takeSeat(int seatIndex, int timeout, TUIRoomDefine.RequestCallback callback) {
        LiveKitLog.info(mTag + " takeSeat:[seatIndex:" + seatIndex + ",timeout:" + timeout + "]");
        return mTUIRoomEngine.takeSeat(seatIndex, timeout, new TUIRoomDefine.RequestCallback() {
            @Override
            public void onAccepted(String requestId, String userId) {
                LiveKitLog.info(mTag + " takeSeat:[onAccepted:[requestId:" + requestId + ",userId:" + userId + "]]");
                if (callback != null) {
                    callback.onAccepted(requestId, userId);
                }
            }

            @Override
            public void onRejected(String requestId, String userId, String message) {
                LiveKitLog.info(mTag + " takeSeat:[onRejected:[requestId:" + requestId + ",userId:" + userId + "]]");
                if (callback != null) {
                    callback.onRejected(requestId, userId, message);
                }
            }

            @Override
            public void onCancelled(String requestId, String userId) {
                LiveKitLog.info(mTag + " takeSeat:[onCancelled:[requestId:" + requestId + ",userId:" + userId + "]]");
                if (callback != null) {
                    callback.onCancelled(requestId, userId);
                }
            }

            @Override
            public void onTimeout(String requestId, String userId) {
                LiveKitLog.info(mTag + " takeSeat:[onTimeout:[requestId:" + requestId + ",userId:" + userId + "]]");
                if (callback != null) {
                    callback.onTimeout(requestId, userId);
                }
            }

            @Override
            public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " takeSeat:onError, [requestId:" + requestId + ",userId:" + userId + ",error:"
                        + error + ",message:" + message + "]");
                if (callback != null) {
                    callback.onError(requestId, userId, error, message);
                }
            }
        });
    }

    public TUIRoomDefine.Request takeUserOnSeatByAdmin(int seatIndex, String userId, int timeout,
                                                       TUIRoomDefine.RequestCallback callback) {
        LiveKitLog.info(mTag + " takeUserOnSeatByAdmin:[seatIndex:" + seatIndex + ",userId:" + userId + ",timeout:"
                + timeout + "]");
        return mTUIRoomEngine.takeUserOnSeatByAdmin(seatIndex, userId, timeout, new TUIRoomDefine.RequestCallback() {
            @Override
            public void onAccepted(String requestId, String userId) {
                LiveKitLog.info(mTag + " takeUserOnSeatByAdmin:[onAccepted:[requestId:" + requestId + ",userId:"
                        + userId + "]]");
                if (callback != null) {
                    callback.onAccepted(requestId, userId);
                }
            }

            @Override
            public void onRejected(String requestId, String userId, String message) {
                LiveKitLog.info(mTag + " takeUserOnSeatByAdmin:[onRejected:[requestId:" + requestId + ",userId:"
                        + userId + "]]");
                if (callback != null) {
                    callback.onRejected(requestId, userId, message);
                }
            }

            @Override
            public void onCancelled(String requestId, String userId) {
                LiveKitLog.info(mTag + " takeUserOnSeatByAdmin:[onCancelled:[requestId:" + requestId + ",userId:"
                        + userId + "]]");
                if (callback != null) {
                    callback.onCancelled(requestId, userId);
                }
            }

            @Override
            public void onTimeout(String requestId, String userId) {
                LiveKitLog.info(mTag + " takeUserOnSeatByAdmin:[onTimeout:[requestId:" + requestId + ",userId:"
                        + userId + "]]");
                if (callback != null) {
                    callback.onTimeout(requestId, userId);
                }
            }

            @Override
            public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " takeUserOnSeatByAdmin:onError, [requestId:" + requestId + ",userId:" + userId
                        + ",error:" + error + ",message:" + message + "]");
                if (callback != null) {
                    callback.onError(requestId, userId, error, message);
                }
            }
        });
    }

    @Override
    public void leaveSeat(TUIRoomDefine.ActionCallback callback) {
        LiveKitLog.info(mTag + " leaveSeat:[]");
        mTUIRoomEngine.leaveSeat(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(mTag + " leaveSeat:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " leaveSeat:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void lockSeat(int seatIndex, TUIRoomDefine.SeatLockParams param, TUIRoomDefine.ActionCallback callback) {
        LiveKitLog.info(mTag + " lockSeatByAdmin:[seatIndex:" + seatIndex + "params:" + new Gson().toJson(param) + "]");
        mTUIRoomEngine.lockSeatByAdmin(seatIndex, param, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(mTag + " lockSeatByAdmin:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " lockSeatByAdmin:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void kickSeat(String userId, TUIRoomDefine.ActionCallback callback) {
        LiveKitLog.info(mTag + " kickSeat:[userId:" + userId + "]");
        mTUIRoomEngine.kickUserOffSeatByAdmin(-1, userId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(mTag + " kickUserOffSeatByAdmin:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " kickUserOffSeatByAdmin:[onError:[error:" + error + ",message:" + message
                        + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void getSeatList(TUIRoomDefine.GetSeatListCallback callback) {
        LiveKitLog.info(mTag + " getSeatList:[]");
        mTUIRoomEngine.getSeatList(new TUIRoomDefine.GetSeatListCallback() {

            @Override
            public void onSuccess(List<TUIRoomDefine.SeatInfo> list) {
                LiveKitLog.info(mTag + " getSeatList:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess(list);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " getSeatList:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void getSeatApplicationList(TUIRoomDefine.RequestListCallback callback) {
        LiveKitLog.info(mTag + " getSeatApplicationList:[]");
        mTUIRoomEngine.getSeatApplicationList(new TUIRoomDefine.RequestListCallback() {
            @Override
            public void onSuccess(List<TUIRoomDefine.Request> list) {
                LiveKitLog.info(mTag + " getSeatApplicationList:[onSuccess:[list:" + new Gson().toJson(list) + "]]");
                if (callback != null) {
                    callback.onSuccess(list);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " getSeatApplicationList:[onError:[error:" + error + ",message:" + message
                        + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void acceptRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        LiveKitLog.info(mTag + " responseRemoteRequest:[requestId:" + requestId + ",agree:" + true + "]");
        mTUIRoomEngine.responseRemoteRequest(requestId, true, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(mTag + " responseRemoteRequest:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " responseRemoteRequest:[onError:[error:" + error + ",message:" + message
                        + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void rejectRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        LiveKitLog.info(mTag + " responseRemoteRequest:[requestId:" + requestId + ",agree:" + false + "]");
        mTUIRoomEngine.responseRemoteRequest(requestId, false, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(mTag + " responseRemoteRequest:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " responseRemoteRequest:[onError:[error:" + error + ",message:" + message
                        + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void cancelRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        LiveKitLog.info(mTag + " cancelRequest:[requestId:" + requestId + "]");
        mTUIRoomEngine.cancelRequest(requestId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(mTag + " cancelRequest:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " cancelRequest:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void kickUserOffSeatByAdmin(int seatIndex, String userId, TUIRoomDefine.ActionCallback callback) {
        LiveKitLog.info(mTag + " kickUserOffSeatByAdmin:[seatIndex:" + seatIndex + "userId:" + userId + "]");
        mTUIRoomEngine.kickUserOffSeatByAdmin(seatIndex, userId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(mTag + " kickUserOffSeatByAdmin:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " kickUserOffSeatByAdmin:[onError:[error:" + error + ",message:" + message
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
        LiveKitLog.info(mTag + " getUserList:[nextSequence:" + nextSequence + "]");
        mTUIRoomEngine.getUserList(nextSequence, new TUIRoomDefine.GetUserListCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserListResult userListResult) {
                LiveKitLog.info(mTag + " getUserList:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess(userListResult);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " getUserList:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void getUserInfo(String userId, TUIRoomDefine.GetUserInfoCallback callback) {
        LiveKitLog.info(mTag + " getUserInfo:[userId:" + userId + "]");
        mTUIRoomEngine.getUserInfo(userId, new TUIRoomDefine.GetUserInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserInfo userInfo) {
                LiveKitLog.info(mTag + " getUserInfo:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess(userInfo);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " getUserInfo:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void muteAllRemoteAudio(boolean isMute) {
        LiveKitLog.info(mTag + " muteAllRemoteAudio:[isMute:" + isMute + "]");
        mTRTCCloud.muteAllRemoteAudio(isMute);
    }

    /****************************************** Media Business *******************************************/
    @Override
    public void openLocalMicrophone(TUIRoomDefine.ActionCallback callback) {
        LiveKitLog.info(mTag + " openLocalMicrophone:[]");
        mTUIRoomEngine.openLocalMicrophone(TUIRoomDefine.AudioQuality.DEFAULT, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(mTag + " openLocalMicrophone:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " openLocalMicrophone:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void openLocalCamera(boolean isFront, TUIRoomDefine.VideoQuality quality,
                                TUIRoomDefine.ActionCallback callback) {
        LiveKitLog.info(mTag + " openLocalCamera:[isFront:" + isFront + "quality:" + quality + "]");
        mTUIRoomEngine.openLocalCamera(isFront, quality, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(mTag + " openLocalCamera:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " openLocalCamera:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void closeLocalCamera() {
        LiveKitLog.info(mTag + " closeLocalCamera:[]");
        mTUIRoomEngine.closeLocalCamera();
    }

    @Override
    public void setCameraMirror(boolean isMirror) {
        LiveKitLog.info(mTag + " setVideoEncoderMirror:[isMirror:" + isMirror + "]");
        TRTCCloudDef.TRTCRenderParams trtcRenderParams = new TRTCCloudDef.TRTCRenderParams();
        trtcRenderParams.mirrorType = isMirror ? TRTC_VIDEO_MIRROR_TYPE_ENABLE : TRTC_VIDEO_MIRROR_TYPE_DISABLE;
        LiveKitLog.info(mTag + " setLocalRenderParams:[trtcRenderParams:" + new Gson().toJson(trtcRenderParams) + "]");
        mTRTCCloud.setLocalRenderParams(trtcRenderParams);
        mTRTCCloud.setVideoEncoderMirror(isMirror);
    }

    @Override
    public void switchCamera(boolean isFrontCamera) {
        LiveKitLog.info(mTag + " switchCamera:[isFrontCamera:" + isFrontCamera + "]");
        mTUIRoomEngine.switchCamera(isFrontCamera);
    }

    @Override
    public void setLocalVideoView(TUIVideoView videoView) {
        LiveKitLog.info(mTag + " setLocalVideoView:[videoView:" + videoView + "]");
        mTUIRoomEngine.setLocalVideoView(videoView);
    }

    @Override
    public void setRemoteVideoView(String userId, TUIRoomDefine.VideoStreamType streamType, TUIVideoView videoView) {
        LiveKitLog.info(mTag + " setRemoteVideoView:[userId:" + userId + "streamType:" + streamType
                + "videoView:" + videoView + "]");
        mTUIRoomEngine.setRemoteVideoView(userId, streamType, videoView);
    }

    @Override
    public void startPlayRemoteVideo(String userId, TUIRoomDefine.VideoStreamType streamType,
                                     TUIRoomDefine.PlayCallback callback) {
        LiveKitLog.info(mTag + " startPlayRemoteVideo:[userId:" + userId + "streamType:" + streamType + "]");
        mTUIRoomEngine.startPlayRemoteVideo(userId, streamType, new TUIRoomDefine.PlayCallback() {
            @Override
            public void onPlaying(String userId) {
                LiveKitLog.info(mTag + " startPlayRemoteVideo:[onPlaying:[userId" + userId + "]]");
                if (callback != null) {
                    callback.onPlaying(userId);
                }
            }

            @Override
            public void onLoading(String userId) {
                LiveKitLog.info(mTag + " startPlayRemoteVideo:[onLoading:[userId" + userId + "]]");
                if (callback != null) {
                    callback.onLoading(userId);
                }
            }

            @Override
            public void onPlayError(String userId, TUICommonDefine.Error error, String message) {
                LiveKitLog.info(mTag + " startPlayRemoteVideo:[onPlayError:[userId" + userId + ",error" + error
                        + ",message" + message + "]]");
                if (callback != null) {
                    callback.onPlayError(userId, error, message);
                }
            }
        });
    }

    @Override
    public void muteLocalAudio() {
        LiveKitLog.info(mTag + " muteLocalAudio:[]");
        mTUIRoomEngine.muteLocalAudio();
    }

    @Override
    public void unMuteLocalAudio(TUIRoomDefine.ActionCallback callback) {
        LiveKitLog.info(mTag + " unMuteLocalAudio:[]");
        mTUIRoomEngine.unmuteLocalAudio(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(mTag + " unMuteLocalAudio:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " unMuteLocalAudio:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void enableGravitySensor(boolean enable) {
        LiveKitLog.info(mTag + " enableGravitySensor:[enable:" + enable + "]");
        mTUIRoomEngine.enableGravitySensor(enable);
    }

    @Override
    public void updateVideoQuality(TUIRoomDefine.VideoQuality quality) {
        LiveKitLog.info(mTag + " updateVideoQuality:[quality:" + quality + "]");
        mTUIRoomEngine.updateVideoQuality(quality);
    }

    @Override
    public void updateAudioQuality(TUIRoomDefine.AudioQuality quality) {
        LiveKitLog.info(mTag + " updateAudioQuality:[quality:" + quality + "]");
        mTUIRoomEngine.updateAudioQuality(quality);
    }

    @Override
    public void setVideoResolutionMode(TUIRoomDefine.ResolutionMode resolutionMode) {
        LiveKitLog.info(mTag + " setVideoResolutionMode:[resolutionMode:" + resolutionMode + "]");
        mTUIRoomEngine.setVideoResolutionMode(TUIRoomDefine.VideoStreamType.CAMERA_STREAM, resolutionMode);
    }

    @Override
    public void setBeautyStyle(int style) {
        LiveKitLog.info(mTag + " setBeautyStyle:[style:" + style + "]");
        mTRTCCloud.getBeautyManager().setBeautyStyle(style);
    }

    @Override
    public void setBeautyLevel(float level) {
        LiveKitLog.info(mTag + " setBeautyLevel:[level:" + level + "]");
        mTRTCCloud.getBeautyManager().setBeautyLevel(level);
    }

    @Override
    public void setWhitenessLevel(float level) {
        LiveKitLog.info(mTag + " setWhitenessLevel:[level:" + level + "]");
        mTRTCCloud.getBeautyManager().setWhitenessLevel(level);
    }

    @Override
    public void setRuddyLevel(float level) {
        LiveKitLog.info(mTag + " setRuddyLevel:[level:" + level + "]");
        mTRTCCloud.getBeautyManager().setRuddyLevel(level);
    }

    /****************************************** IM Business *******************************************/
    @Override
    public void followUser(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowOperationResult>> callback) {
        LiveKitLog.info(mTag + " followUser:[userIDList:" + userIDList + "]");
        V2TIMManager.getFriendshipManager().followUser(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowOperationResult>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowOperationResult> results) {
                        LiveKitLog.info(mTag + " followUser:[onSuccess:[results:" + new Gson().toJson(results) + "]]");
                        if (callback != null) {
                            callback.onSuccess(results);
                        }
                    }

                    @Override
                    public void onError(int code, String message) {
                        LiveKitLog.error(mTag + " followUser:[onSuccess:[code:" + code + ",message:" + message + "]]");
                        if (callback != null) {
                            callback.onError(code, message);
                        }
                    }
                });
    }

    @Override
    public void unfollowUser(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowOperationResult>> callback) {
        LiveKitLog.info(mTag + " unfollowUser:[userIDList:" + userIDList + "]");
        V2TIMManager.getFriendshipManager().unfollowUser(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowOperationResult>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowOperationResult> results) {
                        LiveKitLog.info(mTag + " unfollowUser:[onSuccess:[results:" + new Gson().toJson(results)
                                + "]]");
                        if (callback != null) {
                            callback.onSuccess(results);
                        }
                    }

                    @Override
                    public void onError(int code, String message) {
                        LiveKitLog.error(mTag + " unfollowUser:[onSuccess:[code:" + code + ",message:" + message
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
        LiveKitLog.info(mTag + " checkFollowType:[userIDList:" + userIDList + "]");
        V2TIMManager.getFriendshipManager().checkFollowType(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowTypeCheckResult>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowTypeCheckResult> results) {
                        LiveKitLog.info(mTag + " checkFollowType:[onSuccess:[results:" + new Gson().toJson(results)
                                + "]]");
                        if (callback != null) {
                            callback.onSuccess(results);
                        }
                    }

                    @Override
                    public void onError(int code, String message) {
                        LiveKitLog.error(mTag + " checkFollowType:[onSuccess:[code:" + code
                                + ",message:" + message + "]]");
                        if (callback != null) {
                            callback.onError(code, message);
                        }
                    }
                });
    }

    @Override
    public void getUserFollowInfo(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowInfo>> callback) {
        LiveKitLog.info(mTag + " getUserFollowInfo:[userIDList:" + userIDList + "]");
        V2TIMManager.getFriendshipManager().getUserFollowInfo(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowInfo>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowInfo> results) {
                        LiveKitLog.info(mTag + " getUserFollowInfo:[onSuccess:[results:" + new Gson().toJson(results)
                                + "]]");
                        if (callback != null) {
                            callback.onSuccess(results);
                        }
                    }

                    @Override
                    public void onError(int code, String message) {
                        LiveKitLog.error(mTag + " getUserFollowInfo:[onSuccess:[code:" + code
                                + ",message:" + message + "]]");
                        if (callback != null) {
                            callback.onError(code, message);
                        }
                    }
                });
    }

    @Override
    public void callExperimentalAPI(String jsonStr, Object param) {
        LiveKitLog.info(mTag + " callExperimentalAPI:[jsonStr:" + jsonStr + "param:" + new Gson().toJson(param) + "]");
        TUIRoomEngineImpl.callExperimentalAPI(jsonStr, param);
    }

    /****************************************** Plugin - Room List *******************************************/
    @Override
    public void fetchLiveList(String cursor, int count, TUILiveListManager.LiveInfoListCallback callback) {
        LiveKitLog.info(mTag + " fetchLiveList:[cursor:" + cursor + ",count:" + count + "]");
        mTUILiveListManager.fetchLiveList(cursor, count, new TUILiveListManager.LiveInfoListCallback() {
            @Override
            public void onSuccess(TUILiveListManager.LiveInfoListResult liveInfoListResult) {
                LiveKitLog.info(mTag + " fetchLiveList:[onSuccess:[liveInfoListResult:"
                        + new Gson().toJson(liveInfoListResult));
                if (callback != null) {
                    callback.onSuccess(liveInfoListResult);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {
                LiveKitLog.error(mTag + " fetchLiveList:[onError:[error:" + error + ",s:" + s + "]]");
                if (callback != null) {
                    callback.onError(error, s);
                }
            }
        });
    }

    @Override
    public void setLiveInfo(LiveInfo liveInfo, List<LiveModifyFlag> flagList, TUIRoomDefine.ActionCallback callback) {
        LiveKitLog.info((mTag + " setLiveInfo:[liveInfo:" + new Gson().toJson(liveInfo) + ",flag:" + flagList + "]"));
        mTUILiveListManager.setLiveInfo(liveInfo, flagList, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(mTag + " setLiveInfo:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(mTag + " setLiveInfo:[onError:[error:" + error + ",message:" + message + "]]");
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

    private TUIRoomEngine createEngine() {
        LiveKitLog.info(mTag + " createEngine:[]");
        TUIRoomEngine roomEngine;
        JSONObject jsonObject = new JSONObject();
        try {
            jsonObject.put("api", "createSubRoom");
            roomEngine = (TUIRoomEngine) TUIRoomEngineImpl.callExperimentalAPI(jsonObject.toString());
            return roomEngine;
        } catch (JSONException e) {
            throw new RuntimeException(e);
        }
    }

    private void destroyEngine() {
        LiveKitLog.info(mTag + " destroyEngine:[]");
        JSONObject jsonObject = new JSONObject();
        try {
            jsonObject.put("api", "destroySubRoom");
            TUIRoomEngineImpl.callExperimentalAPI(jsonObject.toString(), mTUIRoomEngine);
        } catch (JSONException e) {
            throw new RuntimeException(e);
        }
    }
}
