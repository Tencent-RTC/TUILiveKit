package com.trtc.uikit.livekit.livestreamcore.manager.api.impl;

import static com.tencent.trtc.TRTCCloudDef.TRTC_VIDEO_MIRROR_TYPE_DISABLE;
import static com.tencent.trtc.TRTCCloudDef.TRTC_VIDEO_MIRROR_TYPE_ENABLE;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.tencent.trtc.TRTCCloud;
import com.tencent.trtc.TRTCCloudDef;
import com.trtc.uikit.livekit.livestreamcore.common.utils.Logger;
import com.trtc.uikit.livekit.livestreamcore.manager.api.ILiveStream;

import java.util.List;
import java.util.Map;

public class LiveStreamImpl implements ILiveStream {
    private final String mTag = "LiveStreamImpl[" + hashCode() + "]";

    private final TUIRoomEngine            mTUIRoomEngine;
    private final TRTCCloud                mTRTCCloud;
    private final TUILiveConnectionManager mTUILiveConnectionManager;

    public LiveStreamImpl() {
        mTUIRoomEngine = TUIRoomEngine.sharedInstance();
        mTRTCCloud = mTUIRoomEngine.getTRTCCloud();
        mTUILiveConnectionManager = mTUIRoomEngine.getLiveConnectionManager();
    }

    @Override
    public void destroy() {
        Logger.info(mTag + " destroy");
        TUIRoomEngine.destroySharedInstance();
    }

    @Override
    public void addRoomEngineObserver(TUIRoomObserver observer) {
        Logger.info(mTag + " addRoomEngineObserver:[observer:" + observer.hashCode() + "]");
        mTUIRoomEngine.addObserver(observer);
    }

    @Override
    public void removeRoomEngineObserver(TUIRoomObserver observer) {
        Logger.info(mTag + " removeRoomEngineObserver:[observer:" + observer.hashCode() + "]");
        mTUIRoomEngine.removeObserver(observer);
    }

    public void addLiveConnectionManagerObserver(TUILiveConnectionManager.Observer observer) {
        Logger.info(mTag + " addLiveConnectionManagerObserver:[observer:" + observer.hashCode() + "]");
        mTUILiveConnectionManager.addObserver(observer);
    }

    public void removeLiveConnectionManagerObserver(TUILiveConnectionManager.Observer observer) {
        Logger.info(mTag + " removeLiveConnectionManagerObserver:[observer:" + observer.hashCode() + "]");
        mTUILiveConnectionManager.removeObserver(observer);
    }

    /****************************************** Room Business *******************************************/
    @Override
    public void createRoom(TUIRoomDefine.RoomInfo roomInfo, TUIRoomDefine.ActionCallback callback) {
        Logger.info(mTag + " createRoom:[roomInfo:" + new Gson().toJson(roomInfo) + "]");
        mTUIRoomEngine.createRoom(roomInfo, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(mTag + " createRoom:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " createRoom:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }


    @Override
    public void enterRoom(String roomId, TUIRoomDefine.GetRoomInfoCallback callback) {
        Logger.info(mTag + " enterRoom:[roomId:" + roomId + ", roomType:" + TUIRoomDefine.RoomType.LIVE + "]");
        mTUIRoomEngine.enterRoom(roomId, TUIRoomDefine.RoomType.LIVE, new TUIRoomDefine.GetRoomInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                Logger.info(mTag + " enterRoom:[onSuccess:[roomInfo" + new Gson().toJson(roomInfo) + "]]");
                if (callback != null) {
                    callback.onSuccess(roomInfo);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " enterRoom:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void exitRoom(TUIRoomDefine.ActionCallback callback) {
        Logger.info(mTag + " exitRoom:[isSyncWaiting" + true + "]");
        mTUIRoomEngine.exitRoom(true, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(mTag + " exitRoom:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " exitRoom:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void destroyRoom(TUIRoomDefine.ActionCallback callback) {
        Logger.info(mTag + " destroyRoom:[]");
        mTUIRoomEngine.destroyRoom(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(mTag + " destroyRoom:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " destroyRoom:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    /****************************************** Seat Business *******************************************/
    @Override
    public TUIRoomDefine.Request takeSeat(int seatIndex, int timeout, TUIRoomDefine.RequestCallback callback) {
        Logger.info(mTag + " takeSeat:[seatIndex:" + seatIndex + ",timeout:" + timeout + "]");
        return mTUIRoomEngine.takeSeat(seatIndex, timeout, new TUIRoomDefine.RequestCallback() {
            @Override
            public void onAccepted(String requestId, String userId) {
                Logger.info(mTag + " takeSeat:[onAccepted:[requestId:" + requestId + ",userId:" + userId + "]]");
                if (callback != null) {
                    callback.onAccepted(requestId, userId);
                }
            }

            @Override
            public void onRejected(String requestId, String userId, String message) {
                Logger.info(mTag + " takeSeat:[onRejected:[requestId:" + requestId + ",userId:" + userId + "]]");
                if (callback != null) {
                    callback.onRejected(requestId, userId, message);
                }
            }

            @Override
            public void onCancelled(String requestId, String userId) {
                Logger.info(mTag + " takeSeat:[onCancelled:[requestId:" + requestId + ",userId:" + userId + "]]");
                if (callback != null) {
                    callback.onCancelled(requestId, userId);
                }
            }

            @Override
            public void onTimeout(String requestId, String userId) {
                Logger.info(mTag + " takeSeat:[onTimeout:[requestId:" + requestId + ",userId:" + userId + "]]");
                if (callback != null) {
                    callback.onTimeout(requestId, userId);
                }
            }

            @Override
            public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " takeSeat:onError, [requestId:" + requestId + ",userId:" + userId + ",error:"
                        + error + ",message:" + message + "]");
                if (callback != null) {
                    callback.onError(requestId, userId, error, message);
                }
            }
        });
    }

    public TUIRoomDefine.Request takeUserOnSeatByAdmin(int seatIndex, String userId, int timeout,
                                                       TUIRoomDefine.RequestCallback callback) {
        Logger.info(mTag + " takeUserOnSeatByAdmin:[seatIndex:" + seatIndex + ",userId:" + userId + ",timeout:"
                + timeout + "]");
        return mTUIRoomEngine.takeUserOnSeatByAdmin(seatIndex, userId, timeout, new TUIRoomDefine.RequestCallback() {
            @Override
            public void onAccepted(String requestId, String userId) {
                Logger.info(mTag + " takeUserOnSeatByAdmin:[onAccepted:[requestId:" + requestId + ",userId:" + userId
                        + "]]");
                if (callback != null) {
                    callback.onAccepted(requestId, userId);
                }
            }

            @Override
            public void onRejected(String requestId, String userId, String message) {
                Logger.info(mTag + " takeUserOnSeatByAdmin:[onRejected:[requestId:" + requestId + ",userId:" + userId
                        + "]]");
                if (callback != null) {
                    callback.onRejected(requestId, userId, message);
                }
            }

            @Override
            public void onCancelled(String requestId, String userId) {
                Logger.info(mTag + " takeUserOnSeatByAdmin:[onCancelled:[requestId:" + requestId + ",userId:" + userId
                        + "]]");
                if (callback != null) {
                    callback.onCancelled(requestId, userId);
                }
            }

            @Override
            public void onTimeout(String requestId, String userId) {
                Logger.info(mTag + " takeUserOnSeatByAdmin:[onTimeout:[requestId:" + requestId + ",userId:" + userId
                        + "]]");
                if (callback != null) {
                    callback.onTimeout(requestId, userId);
                }
            }

            @Override
            public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " takeUserOnSeatByAdmin:onError, [requestId:" + requestId + ",userId:" + userId
                        + ",error:" + error + ",message:" + message + "]");
                if (callback != null) {
                    callback.onError(requestId, userId, error, message);
                }
            }
        });
    }

    @Override
    public void leaveSeat(TUIRoomDefine.ActionCallback callback) {
        Logger.info(mTag + " leaveSeat:[]");
        mTUIRoomEngine.leaveSeat(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(mTag + " leaveSeat:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " leaveSeat:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void lockSeat(int seatIndex, TUIRoomDefine.SeatLockParams param, TUIRoomDefine.ActionCallback callback) {
        Logger.info(mTag + " lockSeatByAdmin:[seatIndex:" + seatIndex + "params:" + new Gson().toJson(param) + "]");
        mTUIRoomEngine.lockSeatByAdmin(seatIndex, param, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(mTag + " lockSeatByAdmin:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " lockSeatByAdmin:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void kickSeat(String userId, TUIRoomDefine.ActionCallback callback) {
        Logger.info(mTag + " kickSeat:[userId:" + userId + "]");
        mTUIRoomEngine.kickUserOffSeatByAdmin(-1, userId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(mTag + " kickUserOffSeatByAdmin:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " kickUserOffSeatByAdmin:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void getSeatList(TUIRoomDefine.GetSeatListCallback callback) {
        Logger.info(mTag + " getSeatList:[]");
        mTUIRoomEngine.getSeatList(new TUIRoomDefine.GetSeatListCallback() {

            @Override
            public void onSuccess(List<TUIRoomDefine.SeatInfo> list) {
                Logger.info(mTag + " getSeatList:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess(list);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " getSeatList:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void getSeatApplicationList(TUIRoomDefine.RequestListCallback callback) {
        Logger.info(mTag + " getSeatApplicationList:[]");
        mTUIRoomEngine.getSeatApplicationList(new TUIRoomDefine.RequestListCallback() {
            @Override
            public void onSuccess(List<TUIRoomDefine.Request> list) {
                Logger.info(mTag + " getSeatApplicationList:[onSuccess:[list:" + new Gson().toJson(list) + "]]");
                if (callback != null) {
                    callback.onSuccess(list);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " getSeatApplicationList:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void acceptRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        Logger.info(mTag + " responseRemoteRequest:[requestId:" + requestId + ",agree:" + true + "]");
        mTUIRoomEngine.responseRemoteRequest(requestId, true, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(mTag + " responseRemoteRequest:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " responseRemoteRequest:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void rejectRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        Logger.info(mTag + " responseRemoteRequest:[requestId:" + requestId + ",agree:" + false + "]");
        mTUIRoomEngine.responseRemoteRequest(requestId, false, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(mTag + " responseRemoteRequest:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " responseRemoteRequest:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void cancelRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        Logger.info(mTag + " cancelRequest:[requestId:" + requestId + "]");
        mTUIRoomEngine.cancelRequest(requestId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(mTag + " cancelRequest:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " cancelRequest:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void kickUserOffSeatByAdmin(int seatIndex, String userId, TUIRoomDefine.ActionCallback callback) {
        Logger.info(mTag + " kickUserOffSeatByAdmin:[seatIndex:" + seatIndex + "userId:" + userId + "]");
        mTUIRoomEngine.kickUserOffSeatByAdmin(seatIndex, userId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(mTag + " kickUserOffSeatByAdmin:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " kickUserOffSeatByAdmin:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    /****************************************** User Business *******************************************/
    @Override
    public void getUserInfo(String userId, TUIRoomDefine.GetUserInfoCallback callback) {
        Logger.info(mTag + " getUserInfo:[userId:" + userId + "]");
        mTUIRoomEngine.getUserInfo(userId, new TUIRoomDefine.GetUserInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserInfo userInfo) {
                Logger.info(mTag + " getUserInfo:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess(userInfo);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " getUserInfo:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    /****************************************** Media Business *******************************************/
    @Override
    public void openLocalMicrophone(TUIRoomDefine.ActionCallback callback) {
        Logger.info(mTag + " openLocalMicrophone:[]");
        mTUIRoomEngine.openLocalMicrophone(TUIRoomDefine.AudioQuality.DEFAULT, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(mTag + " openLocalMicrophone:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " openLocalMicrophone:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void closeLocalMicrophone() {
        Logger.info(mTag + " closeLocalMicrophone:[]");
        mTUIRoomEngine.closeLocalMicrophone();
    }

    @Override
    public void openLocalCamera(boolean isFront, TUIRoomDefine.VideoQuality quality,
                                TUIRoomDefine.ActionCallback callback) {
        Logger.info(mTag + " openLocalCamera:[isFront:" + isFront + "quality:" + quality + "]");
        mTUIRoomEngine.openLocalCamera(isFront, quality, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(mTag + " openLocalCamera:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " openLocalCamera:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void closeLocalCamera() {
        Logger.info(mTag + " closeLocalCamera:[]");
        mTUIRoomEngine.closeLocalCamera();
    }

    @Override
    public void switchCamera(boolean isFrontCamera) {
        Logger.info(mTag + " switchCamera:[isFrontCamera:" + isFrontCamera + "]");
        mTUIRoomEngine.switchCamera(isFrontCamera);
    }

    @Override
    public void setLocalVideoView(TUIVideoView videoView) {
        Logger.info(mTag + " setLocalVideoView:[videoView:" + videoView + "]");
        mTUIRoomEngine.setLocalVideoView(videoView);
    }

    @Override
    public void setRemoteVideoView(String userId, TUIRoomDefine.VideoStreamType streamType, TUIVideoView videoView) {
        Logger.info(mTag + " setRemoteVideoView:[userId:" + userId + "streamType:" + streamType
                + "videoView:" + videoView + "]");
        mTUIRoomEngine.setRemoteVideoView(userId, streamType, videoView);
    }

    @Override
    public void startPlayRemoteVideo(String userId, TUIRoomDefine.VideoStreamType streamType,
                                     TUIRoomDefine.PlayCallback callback) {
        Logger.info(mTag + " startPlayRemoteVideo:[userId:" + userId + "streamType:" + streamType + "]");
        mTUIRoomEngine.startPlayRemoteVideo(userId, streamType, new TUIRoomDefine.PlayCallback() {
            @Override
            public void onPlaying(String userId) {
                Logger.info(mTag + " startPlayRemoteVideo:[onPlaying:[userId" + userId + "]]");
                if (callback != null) {
                    callback.onPlaying(userId);
                }
            }

            @Override
            public void onLoading(String userId) {
                Logger.info(mTag + " startPlayRemoteVideo:[onLoading:[userId" + userId + "]]");
                if (callback != null) {
                    callback.onLoading(userId);
                }
            }

            @Override
            public void onPlayError(String userId, TUICommonDefine.Error error, String message) {
                Logger.info(mTag + " startPlayRemoteVideo:[onPlayError:[userId" + userId + ",error" + error
                        + ",message" + message + "]]");
                if (callback != null) {
                    callback.onPlayError(userId, error, message);
                }
            }
        });
    }

    @Override
    public void muteLocalAudio() {
        Logger.info(mTag + " muteLocalAudio:[]");
        mTUIRoomEngine.muteLocalAudio();
    }

    @Override
    public void unMuteLocalAudio(TUIRoomDefine.ActionCallback callback) {
        Logger.info(mTag + " unMuteLocalAudio:[]");
        mTUIRoomEngine.unmuteLocalAudio(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(mTag + " unMuteLocalAudio:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " unMuteLocalAudio:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void enableGravitySensor(boolean enable) {
        Logger.info(mTag + " enableGravitySensor:[enable:" + enable + "]");
        mTUIRoomEngine.enableGravitySensor(enable);
    }

    @Override
    public void setVideoResolutionMode(TUIRoomDefine.ResolutionMode resolutionMode) {
        Logger.info(mTag + " setVideoResolutionMode:[resolutionMode:" + resolutionMode + "]");
        mTUIRoomEngine.setVideoResolutionMode(TUIRoomDefine.VideoStreamType.CAMERA_STREAM, resolutionMode);
    }

    @Override
    public void setBeautyStyle(int style) {
        Logger.info(mTag + " setBeautyStyle:[style:" + style + "]");
        mTRTCCloud.getBeautyManager().setBeautyStyle(style);
    }

    @Override
    public void updateVideoQuality(TUIRoomDefine.VideoQuality quality) {
        Logger.info(mTag + " updateVideoQuality:[quality:" + quality + "]");
        mTUIRoomEngine.updateVideoQuality(quality);
    }

    @Override
    public void updateAudioQuality(TUIRoomDefine.AudioQuality quality) {
        Logger.info(mTag + " updateAudioQuality:[quality:" + quality + "]");
        mTUIRoomEngine.updateAudioQuality(quality);
    }

    @Override
    public void setCameraMirror(boolean isMirror) {
        Logger.info(mTag + " setVideoEncoderMirror:[isMirror:" + isMirror + "]");
        TRTCCloudDef.TRTCRenderParams trtcRenderParams = new TRTCCloudDef.TRTCRenderParams();
        trtcRenderParams.mirrorType = isMirror ? TRTC_VIDEO_MIRROR_TYPE_ENABLE : TRTC_VIDEO_MIRROR_TYPE_DISABLE;
        Logger.info(mTag + " setLocalRenderParams:[trtcRenderParams:" + new Gson().toJson(trtcRenderParams) + "]");
        mTRTCCloud.setLocalRenderParams(trtcRenderParams);
        mTRTCCloud.setVideoEncoderMirror(isMirror);
    }

    @Override
    public void callExperimentalAPI(String jsonStr) {
        Logger.info(mTag + " callExperimentalAPI:[jsonStr:" + jsonStr+ "]");
        TUIRoomEngine.callExperimentalAPI(jsonStr);
    }

    @Override
    public TRTCCloud getTRTCCloud() {
        return mTRTCCloud;
    }

    public void requestConnection(List<String> roomIdList, int timeoutSeconds, String extensionInfo,
                                  TUILiveConnectionManager.ConnectionRequestCallback callback) {
        Logger.info(mTag + " requestConnection:[roomIdList:" + roomIdList + ",timeoutSeconds:" + timeoutSeconds
                + ",extensionInfo:" + extensionInfo + "]");
        mTUILiveConnectionManager.requestConnection(roomIdList, timeoutSeconds, extensionInfo,
                new TUILiveConnectionManager.ConnectionRequestCallback() {
                    @Override
                    public void onSuccess(Map<String, TUILiveConnectionManager.ConnectionCode> resultMap) {
                        Logger.info(mTag + " requestConnection:[onSuccess:[resultMap:" + new Gson().toJson(resultMap)
                                + "]]");
                        if (callback != null) {
                            callback.onSuccess(resultMap);
                        }
                    }

                    @Override
                    public void onError(TUICommonDefine.Error error, String message) {
                        Logger.error(mTag + " requestConnection:[onError:[error:" + error + ",message:" + message
                                + "]]");
                        if (callback != null) {
                            callback.onError(error, message);
                        }
                    }
                });
    }

    public void acceptConnection(String roomId, TUIRoomDefine.ActionCallback callback) {
        Logger.info(mTag + " acceptConnection:[roomId:" + roomId + "]");
        mTUILiveConnectionManager.acceptConnection(roomId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(mTag + " acceptConnection:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " acceptConnection:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    public void rejectConnection(String roomId, TUIRoomDefine.ActionCallback callback) {
        Logger.info(mTag + " rejectConnection:[roomId:" + roomId + "]");
        mTUILiveConnectionManager.rejectConnection(roomId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(mTag + " rejectConnection:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " rejectConnection:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    public void disconnect(TUIRoomDefine.ActionCallback callback) {
        Logger.info(mTag + " disconnect:[]");
        mTUILiveConnectionManager.disconnect(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(mTag + " disconnect:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " disconnect:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    public void cancelConnectionRequest(List<String> roomIdList, TUIRoomDefine.ActionCallback callback) {
        Logger.info(mTag + " cancelConnectionRequest:[roomIdList:" + roomIdList + "]");
        mTUILiveConnectionManager.cancelConnectionRequest(roomIdList, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(mTag + " cancelConnectionRequest:[onSuccess]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(mTag + " cancelConnectionRequest:[onError:[error:" + error + ",message:" + message + "]]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

}
