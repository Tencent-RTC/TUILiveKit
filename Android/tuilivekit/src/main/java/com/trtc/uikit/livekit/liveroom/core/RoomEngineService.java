package com.trtc.uikit.livekit.liveroom.core;

import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.AudioQuality.DEFAULT;
import static com.tencent.liteav.beauty.TXBeautyManager.TXBeautyStyleSmooth;
import static com.tencent.trtc.TRTCCloudDef.TRTC_VIDEO_MIRROR_TYPE_DISABLE;
import static com.tencent.trtc.TRTCCloudDef.TRTC_VIDEO_MIRROR_TYPE_ENABLE;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.liteav.audio.TXAudioEffectManager;
import com.tencent.trtc.TRTCCloud;
import com.tencent.trtc.TRTCCloudDef;
import com.trtc.uikit.livekit.common.core.store.LiveStore;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.data.MusicInfo;
import com.trtc.uikit.livekit.liveroom.data.UserInfo;
import com.trtc.uikit.livekit.liveroom.view.common.video.VideoViewFactory;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;

public class RoomEngineService {
    private LiveRoomInfo   mLiveRoomInfo;
    private TUIRoomEngine  mRoomEngine;
    private EngineObserver mEngineObserver;

    public RoomEngineService(LiveRoomInfo liveRoomInfo) {
        mLiveRoomInfo = liveRoomInfo;
        mRoomEngine = TUIRoomEngine.sharedInstance();
        mEngineObserver = new EngineObserver(mLiveRoomInfo, this);
        mRoomEngine.addObserver(mEngineObserver);
    }

    public void destroy() {
        if (mRoomEngine != null && mEngineObserver != null) {
            mRoomEngine.removeObserver(mEngineObserver);
            mEngineObserver = null;
        }
        if (mRoomEngine != null) {
            mRoomEngine.destroySharedInstance();
            mRoomEngine = null;
        }
    }

    public void createRoom(TUIRoomDefine.RoomInfo roomInfo, TUIRoomDefine.ActionCallback callback) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] createRoom:[roomInfo:" + roomInfo);
        mRoomEngine.createRoom(roomInfo, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveStore.sharedInstance().getLiveController().getRoomController()
                        .updateCreateTime(System.currentTimeMillis());
                LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] createRoom:[Success]");
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error("RoomEngineService[" + mLiveRoomInfo.roomId + "] createRoom:[Error] error:"
                        + error + ",message:" + message + "]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    public void enterRoom(String streamId, TUIRoomDefine.GetRoomInfoCallback callback) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] enterRoom:[streamId:" + streamId + "]");
        mRoomEngine.enterRoom(streamId, TUIRoomDefine.RoomType.LIVE, new TUIRoomDefine.GetRoomInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] enterRoom:[Success]");
                mLiveRoomInfo.maxSeatCount = roomInfo.maxSeatCount;
                getSeatList();
                getAudienceList();
                if (callback != null) {
                    callback.onSuccess(roomInfo);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error("RoomEngineService[" + mLiveRoomInfo.roomId + "] enterRoom:[Error] error:"
                        + error + ",message:" + message + "]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }


    public void exitRoom(TUIRoomDefine.ActionCallback callback) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] exitRoom:[roomId=" + mLiveRoomInfo.roomId
                + "]");
        mRoomEngine.exitRoom(false, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] exitRoom:[Success]");
                VideoViewFactory.instance.clear();
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error("RoomEngineService[" + mLiveRoomInfo.roomId + "] exitRoom:[Error] error:" + error
                        + ",message:" + message + "]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    public void destroyRoom(TUIRoomDefine.ActionCallback callback) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] destroyRoom");
        mRoomEngine.destroyRoom(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] destroyRoom:[Success]");
                VideoViewFactory.instance.clear();
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error("RoomEngineService[" + mLiveRoomInfo.roomId + "] destroyRoom:[Error] error:" + error
                        + ",message:" + message + "]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    public void initLivingConfig() {
        mRoomEngine.enableGravitySensor(true);
        setVideoResolutionMode();
        updateVideoQuality(LiveKitStore.sharedInstance().selfInfo.videoInfo.videoQuality.get());

        setBeautyStyle(TXBeautyStyleSmooth);
        setBeautyLevel(LiveKitStore.sharedInstance().selfInfo.beautyInfo.smoothLevel.get());
        setWhitenessLevel(LiveKitStore.sharedInstance().selfInfo.beautyInfo.whitenessLevel.get());
        setRuddyLevel(LiveKitStore.sharedInstance().selfInfo.beautyInfo.ruddyLevel.get());

        updateAudioQuality(LiveKitStore.sharedInstance().selfInfo.audioInfo.audioQuality.get());
    }

    public void getRoomInfo(TUIRoomDefine.GetRoomInfoCallback callback) {
        mRoomEngine.fetchRoomInfo(new TUIRoomDefine.GetRoomInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                mLiveRoomInfo.audienceCount.set(roomInfo.memberCount > 0 ? roomInfo.memberCount - 1 : 0);
                mLiveRoomInfo.roomId = roomInfo.roomId;
                mLiveRoomInfo.name.set(roomInfo.name);
                mLiveRoomInfo.coverURL.set("https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/"
                        + "voice_room/voice_room_cover1.png");
                if (callback != null) {
                    callback.onSuccess(roomInfo);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error("RoomEngineService[" + mLiveRoomInfo.roomId + "] getRoomInfo:[Error] error:" + error
                        + ",message:" + message + "]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    public void getSeatList() {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] getSeatList");
        mRoomEngine.getSeatList(new TUIRoomDefine.GetSeatListCallback() {
            @Override
            public void onSuccess(List<TUIRoomDefine.SeatInfo> list) {
                LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] getSeatList:[Success, list:"
                        + list + "]");
                handleSeatChange(list, new ArrayList<>());
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error("RoomEngineService[" + mLiveRoomInfo.roomId + "] getSeatList:[Error] error:" + error
                        + ", message:" + message + "]");
            }
        });
    }


    public void leaveSeat() {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] leaveSeat");
        mRoomEngine.leaveSeat(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] leaveSeat:[Success]");
                LiveKitStore.sharedInstance().setSelfStatus(TUILiveDefine.UserInteractionStatus.NONE);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error("RoomEngineService[" + mLiveRoomInfo.roomId + "] leaveSeat:[Error] error:" + error
                        + ",message:" + message + "]");
            }
        });
    }

    public void cancelRequest(String requestId) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] cancelRequest:[requestId:" + requestId + "]");
        mRoomEngine.cancelRequest(requestId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] cancelRequest:[Success]");
                LiveKitStore.sharedInstance().setSelfStatus(TUILiveDefine.UserInteractionStatus.NONE);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error("RoomEngineService[" + mLiveRoomInfo.roomId + "] cancelRequest:[Error] error:"
                        + error + ",message:" + message + "]");
            }
        });
    }

    public void getAudienceList() {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] getAudienceList");
        mRoomEngine.getUserList(0, new TUIRoomDefine.GetUserListCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserListResult userListResult) {
                LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] getAudienceList:[Success, "
                        + "userListResult:" + userListResult + "]");
                if (userListResult.userInfoList.size() > 0) {
                    mLiveRoomInfo.audienceList.get().clear();
                    Set<UserInfo> userInfoSet = new LinkedHashSet<>();
                    for (TUIRoomDefine.UserInfo userInfo : userListResult.userInfoList) {
                        if (userInfo.userId.equals(mLiveRoomInfo.anchorInfo.userId)) {
                            continue;
                        }
                        UserInfo liveUserInfo = new UserInfo(userInfo);
                        userInfoSet.add(liveUserInfo);
                    }
                    mLiveRoomInfo.audienceList.addAll(userInfoSet);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error("RoomEngineService[" + mLiveRoomInfo.roomId + "] getAudienceList:[Error] error:"
                        + error + ",message:" + message + "]");
            }
        });
    }

    public void setLocalVideoView(TUIVideoView view) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] setLocalVideoView:[view:" + view + "]");
        mRoomEngine.setLocalVideoView(view);
    }


    public void openLocalCamera(boolean isFront, TUIRoomDefine.VideoQuality quality,
                                TUIRoomDefine.ActionCallback callback) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] openLocalCamera:[isFront:" + isFront
                + ",quality:" + quality + "]");
        mRoomEngine.openLocalCamera(isFront, quality, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] openLocalCamera:[Success]");
                LiveKitStore.sharedInstance().selfInfo.videoInfo.isCameraOpened.set(true);
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error("RoomEngineService[" + mLiveRoomInfo.roomId + "] openLocalCamera:[Error] error:"
                        + error + ",message:" + message + "]");
                LiveKitStore.sharedInstance().selfInfo.videoInfo.isCameraOpened.set(false);
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    public void closeLocalCamera() {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] closeLocalCamera");
        mRoomEngine.closeLocalCamera();
    }

    public void closeLocalMicrophone() {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] closeLocalMicrophone");
        mRoomEngine.closeLocalMicrophone();
    }

    public void startPlayRemoteVideo(String userId, TUIRoomDefine.VideoStreamType streamType,
                                     TUIRoomDefine.PlayCallback callback) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] startPlayRemoteVideo:[userId:" + userId
                + ",streamType:" + streamType + "]");
        mRoomEngine.startPlayRemoteVideo(userId, streamType, callback);
    }

    public void setRemoteVideoView(String userId, TUIRoomDefine.VideoStreamType streamType, TUIVideoView videoView) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] setRemoteVideoView:[userId:" + userId
                + ",streamType:" + streamType + ",videoView:" + videoView + "]");
        mRoomEngine.setRemoteVideoView(userId, streamType, videoView);
    }

    public void switchCamera() {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] switchCamera");
        LiveKitStore.sharedInstance().selfInfo.videoInfo.isFrontCamera
                .set(!LiveKitStore.sharedInstance().selfInfo.videoInfo.isFrontCamera.get());
        mRoomEngine.switchCamera(LiveKitStore.sharedInstance().selfInfo.videoInfo.isFrontCamera.get());
    }

    public void setCameraMirror() {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] setCameraMirror");
        boolean isMirror = !LiveKitStore.sharedInstance().selfInfo.videoInfo.isMirror.get();
        TRTCCloudDef.TRTCRenderParams trtcRenderParams = new TRTCCloudDef.TRTCRenderParams();
        trtcRenderParams.mirrorType = isMirror ? TRTC_VIDEO_MIRROR_TYPE_ENABLE : TRTC_VIDEO_MIRROR_TYPE_DISABLE;
        getTRTCCloud().setLocalRenderParams(trtcRenderParams);
        getTRTCCloud().setVideoEncoderMirror(isMirror);
        LiveKitStore.sharedInstance().selfInfo.videoInfo.isMirror.set(isMirror);
    }

    public void setBeautyStyle(int style) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] setBeautyStyle:[style:" + style + "]");
        getTRTCCloud().getBeautyManager().setBeautyStyle(style);
    }

    public void muteAllRemoteAudio(boolean isMute) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] muteAllRemoteAudio:[isMute:"
                + isMute + "]");
        getTRTCCloud().muteAllRemoteAudio(isMute);
    }

    public void setBeautyLevel(float level) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] setBeautyLevel:[level:" + level + "]");
        getTRTCCloud().getBeautyManager().setBeautyLevel(level);
    }

    public void setWhitenessLevel(float level) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] setWhitenessLevel:[level:" + level + "]");
        getTRTCCloud().getBeautyManager().setWhitenessLevel(level);
    }

    public void setRuddyLevel(float level) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] setRuddyLevel:[level:" + level + "]");
        getTRTCCloud().getBeautyManager().setRuddyLevel(level);
    }

    public TUIRoomDefine.Request takeSeat(int seatIndex, int timeOut, TUIRoomDefine.RequestCallback callback) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] takeSeat:[seatIndex:" + seatIndex
                + ",timeOut:" + timeOut + "]");
        return mRoomEngine.takeSeat(seatIndex, timeOut, new TUIRoomDefine.RequestCallback() {
            @Override
            public void onAccepted(String requestId, String userId) {
                LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] takeSeat:[Accepted, requestId:"
                        + requestId + ",userId:" + userId);
                if (callback != null) {
                    callback.onAccepted(requestId, userId + "]");
                }
            }

            @Override
            public void onRejected(String requestId, String userId, String message) {
                LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] takeSeat:[Rejected, requestId:"
                        + requestId + ",userId:" + userId + ",message:" + message + "]");
                if (callback != null) {
                    callback.onRejected(requestId, userId, message);
                }
            }

            @Override
            public void onCancelled(String requestId, String userId) {
                LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] takeSeat:[Cancelled, requestId:"
                        + requestId + ",userId:" + userId + "]");
                if (callback != null) {
                    callback.onCancelled(requestId, userId);
                }
            }

            @Override
            public void onTimeout(String requestId, String userId) {
                LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] takeSeat:[Timeout, requestId:"
                        + requestId + ",userId:" + userId + "]");
                if (callback != null) {
                    callback.onTimeout(requestId, userId);
                }
            }

            @Override
            public void onError(String requestId, String userId, TUICommonDefine.Error code, String message) {
                LiveKitLog.error("RoomEngineService[" + mLiveRoomInfo.roomId + "] takeSeat:[Error, requestId:"
                        + requestId + ",userId:" + userId + ",code:" + code + ".message:" + message + "]");
                if (callback != null) {
                    callback.onError(requestId, userId, code, message);
                }
            }
        });
    }

    public void responseRemoteRequestUser(UserInfo userInfo, boolean agree, TUIRoomDefine.ActionCallback callback) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] responseRemoteRequestUser:[userInfo:"
                + userInfo + ",agree:" + agree + "]");
        mRoomEngine.responseRemoteRequest(userInfo.requestId, agree, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] responseRemoteRequestUser:[Success]");
                LiveKitStore.sharedInstance().applyLinkAudienceList.remove(userInfo);
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error("RoomEngineService[" + mLiveRoomInfo.roomId + "] responseRemoteRequestUser:[Error] "
                        + "error:" + error + ",message:" + message + "]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    public void kickUserOffSeatByAdmin(int seatIndex, UserInfo userInfo, TUIRoomDefine.ActionCallback callback) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] kickUserOffSeatByAdmin:[seatIndex:"
                + seatIndex + ",userInfo:" + userInfo + "]");
        mRoomEngine.kickUserOffSeatByAdmin(seatIndex, userInfo.userId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] kickUserOffSeatByAdmin:[Success]");
                mLiveRoomInfo.linkingAudienceList.remove(userInfo);
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error("RoomEngineService[" + mLiveRoomInfo.roomId + "] kickUserOffSeatByAdmin:[Error] error:"
                        + error + ",message:" + message + "]");
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    public void enableLinkMicRequest(boolean enable) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] enableLinkMicRequest:[enable:" + enable + "]");
        if (enable) {
            mLiveRoomInfo.interactionType.set(TUILiveDefine.InteractionType.LINK);
        } else {
            mLiveRoomInfo.interactionType.set(TUILiveDefine.InteractionType.BROADCAST);
        }
    }

    public void openLocalMicrophone() {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] openLocalMicrophone");
        mRoomEngine.openLocalMicrophone(DEFAULT, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] openLocalMicrophone:[Success]");
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error("RoomEngineService[" + mLiveRoomInfo.roomId + "] openLocalMicrophone:[Error] error:"
                        + error + ",message:" + message + "]");
            }
        });
    }

    public void startMusic(MusicInfo musicInfo) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] startMusic:[musicInfo:" + musicInfo + "]");
        TXAudioEffectManager.AudioMusicParam audioMusicParam = new TXAudioEffectManager.AudioMusicParam(musicInfo.id,
                musicInfo.path);
        audioMusicParam.loopCount = Integer.MAX_VALUE;
        audioMusicParam.publish = true;
        getTRTCCloud().getAudioEffectManager().startPlayMusic(audioMusicParam);
        getTRTCCloud().getAudioEffectManager().setAllMusicVolume(mLiveRoomInfo.anchorInfo.audioInfo.musicVolume.get());
        getTRTCCloud().getAudioEffectManager().setMusicPitch(musicInfo.id, musicInfo.pitch.get());
    }

    public void pauseMusic(int id) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] pauseMusic:[id:" + id + "]");
        getTRTCCloud().getAudioEffectManager().pausePlayMusic(id);
    }

    public void stopMusic(int id) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] stopMusic:[id:" + id + "]");
        getTRTCCloud().getAudioEffectManager().stopPlayMusic(id);
    }

    public void updateVideoQuality(TUIRoomDefine.VideoQuality quality) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] updateVideoQuality:[quality:" + quality + "]");
        mRoomEngine.updateVideoQuality(quality);
    }

    private void updateAudioQuality(TUIRoomDefine.AudioQuality quality) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] updateAudioQuality:[quality:" + quality + "]");
        mRoomEngine.updateAudioQuality(quality);
    }

    public void setVoiceChangerType(TXAudioEffectManager.TXVoiceChangerType type) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] setVoiceChangerType:[type:" + type + "]");
        getTRTCCloud().getAudioEffectManager().setVoiceChangerType(type);
    }

    public void setVoiceReverbType(TXAudioEffectManager.TXVoiceReverbType type) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] setVoiceReverbType:[type:" + type + "]");
        getTRTCCloud().getAudioEffectManager().setVoiceReverbType(type);
    }

    public void enableVoiceEarMonitor(boolean enable) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] enableVoiceEarMonitor:[enable:" + enable
                + "]");
        getTRTCCloud().getAudioEffectManager().enableVoiceEarMonitor(enable);
    }

    public void setVoiceEarMonitorVolume(int volume) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] setVoiceEarMonitorVolume:[volume:" + volume
                + "]");
        getTRTCCloud().getAudioEffectManager().setVoiceEarMonitorVolume(volume);
    }

    public void setMusicVolume(int id, int volume) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] setMusicVolume:[id:" + id + ",volume:"
                + volume + "]");
        getTRTCCloud().getAudioEffectManager().setMusicPlayoutVolume(id, volume);
        getTRTCCloud().getAudioEffectManager().setMusicPublishVolume(id, volume);
    }

    public void setVoiceVolume(int volume) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] setVoiceVolume:[volume:" + volume + "]");
        getTRTCCloud().getAudioEffectManager().setVoiceCaptureVolume(volume);
    }

    public void setMusicPitch(int id, float pitch) {
        LiveKitLog.info("RoomEngineService[" + mLiveRoomInfo.roomId + "] setMusicPitch:[id:" + id + ",pitch:" + pitch
                + "]");
        getTRTCCloud().getAudioEffectManager().setMusicPitch(id, pitch);
    }

    private void setVideoResolutionMode() {
        mRoomEngine.setVideoResolutionMode(TUIRoomDefine.VideoStreamType.CAMERA_STREAM,
                LiveKitStore.sharedInstance().isPortrait.get()
                        ? TUIRoomDefine.ResolutionMode.PORTRAIT : TUIRoomDefine.ResolutionMode.LANDSCAPE);
    }

    private TRTCCloud getTRTCCloud() {
        return mRoomEngine.getTRTCCloud();
    }

    public void handleSeatChange(List<TUIRoomDefine.SeatInfo> seatList, List<TUIRoomDefine.SeatInfo> new1List) {
        CopyOnWriteArrayList<TUIRoomDefine.SeatInfo> safeSeatList = new CopyOnWriteArrayList();
        CopyOnWriteArrayList<TUIRoomDefine.SeatInfo> safeSeatedList = new CopyOnWriteArrayList();
        safeSeatList.addAll(seatList);
        safeSeatedList.addAll(new1List);

        for (TUIRoomDefine.SeatInfo seatInfo : safeSeatList) {
            if (seatInfo.userId.isEmpty()) {
                safeSeatList.remove(seatInfo);
            }
            boolean hasSeated = false;
            for (TUIRoomDefine.SeatInfo seatedInfo : safeSeatedList) {
                if (seatedInfo.userId.equals(seatInfo.userId)) {
                    hasSeated = true;
                    break;
                }
            }
            if (hasSeated) {
                safeSeatList.remove(seatInfo);
            }
        }
        safeSeatList.addAll(safeSeatedList);


        List<TUIRoomDefine.SeatInfo> seatedList = new ArrayList<>();
        for (TUIRoomDefine.SeatInfo seatInfo : safeSeatList) {
            if (!mLiveRoomInfo.linkingAudienceList.get().contains(new UserInfo(seatInfo.userId))) {
                seatedList.add(seatInfo);
            }
        }
        handleHasSeated(seatedList);

        List<TUIRoomDefine.SeatInfo> leftList = new ArrayList<>();
        for (UserInfo userInfo : mLiveRoomInfo.linkingAudienceList.get()) {
            boolean hasSeat = false;
            for (TUIRoomDefine.SeatInfo seatInfo : safeSeatList) {
                if (seatInfo.userId.equals(userInfo.userId)) {
                    hasSeat = true;
                    break;
                }
            }
            if (!hasSeat) {
                TUIRoomDefine.SeatInfo seatInfo = new TUIRoomDefine.SeatInfo();
                seatInfo.userId = userInfo.userId;
                leftList.add(seatInfo);
            }
        }
        handleLeftSeat(leftList);
    }

    private void handleHasSeated(List<TUIRoomDefine.SeatInfo> seatedList) {
        for (TUIRoomDefine.SeatInfo item : seatedList) {
            if (item.userId.equals(mLiveRoomInfo.anchorInfo.userId)) {
                continue;
            }
            UserInfo userInf = new UserInfo();
            userInf.userId = item.userId;
            userInf.name.set(item.userName);
            userInf.avatarUrl.set(item.avatarUrl);
            if (mLiveRoomInfo.hasVideoList.contains(userInf.userId)) {
                userInf.videoInfo.isCameraOpened.set(true);
            } else {
                userInf.videoInfo.isCameraOpened.set(false);
            }
            if (mLiveRoomInfo.hasAudioList.contains(userInf.userId)) {
                userInf.audioInfo.muteAudio.set(false);
            } else {
                userInf.audioInfo.muteAudio.set(true);
            }
            mLiveRoomInfo.linkingAudienceList.add(userInf);
        }
    }

    private void handleLeftSeat(List<TUIRoomDefine.SeatInfo> left) {
        CopyOnWriteArrayList<TUIRoomDefine.SeatInfo> safeLeftList = new CopyOnWriteArrayList<>(left);
        for (TUIRoomDefine.SeatInfo leftInfo : safeLeftList) {
            for (UserInfo userInfo : mLiveRoomInfo.linkingAudienceList.get()) {
                if (userInfo.userId.equals(leftInfo.userId)) {
                    mLiveRoomInfo.linkingAudienceList.remove(userInfo);
                }
            }
            if (leftInfo.userId.equals(LiveKitStore.sharedInstance().selfInfo.userId)) {
                LiveKitStore.sharedInstance().selfInfo.videoInfo.isCameraOpened.set(false);
                LiveKitStore.sharedInstance().setSelfStatus(TUILiveDefine.UserInteractionStatus.NONE);
            }
        }
    }
}
