package com.trtc.uikit.livekit.livestreamcore.state;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.RoomVideoEncoderParams;
import com.trtc.tuikit.common.livedata.LiveData;

public class MediaState {
    public LiveData<Boolean>   hasMicrophonePermission = new LiveData<>(false);
    public LiveData<Boolean>   isMicrophoneOpened      = new LiveData<>(false);
    public LiveData<Boolean>   isMicrophoneMuted       = new LiveData<>(true);
    public LiveData<Boolean>   hasCameraPermission     = new LiveData<>(false);
    public LiveData<Boolean>   isCameraOpened          = new LiveData<>(false);
    public LiveData<Boolean>   isFrontCamera           = new LiveData<>(true);
    public LiveData<Boolean>   isMirror                = new LiveData<>(true);
    public VideoAdvanceSetting videoAdvanceSetting     = new VideoAdvanceSetting();
    public VideoEncParams      videoEncParams          = new VideoEncParams();


    public static class VideoAdvanceSetting {
        public boolean isVisible         = false;
        public boolean isUltimateEnabled = false;
        public boolean isH265Enabled     = false;
    }


    public static class VideoEncParams {

        public RoomVideoEncoderParams big;
        public RoomVideoEncoderParams small;
        public VideoEncType           currentEncType;


        public enum VideoEncType {

            BIG,
            SMALL
        }

        public VideoEncParams() {
            big = new RoomVideoEncoderParams();
            big.videoResolution = TUIRoomDefine.VideoQuality.Q_1080P;
            big.bitrate = 6000;
            big.fps = 30;
            big.resolutionMode = TUIRoomDefine.ResolutionMode.PORTRAIT;

            small = new RoomVideoEncoderParams();
            small.videoResolution = TUIRoomDefine.VideoQuality.Q_540P;
            small.bitrate = 1500;
            small.fps = 30;
            small.resolutionMode = TUIRoomDefine.ResolutionMode.PORTRAIT;
        }

        public RoomVideoEncoderParams getCurrentEnc() {
            if (currentEncType == VideoEncType.SMALL) {
                return small;
            }
            return big;
        }

        public void setCurrentEnc(RoomVideoEncoderParams videoEncParams) {
            RoomVideoEncoderParams currentParams = (currentEncType == VideoEncType.SMALL) ? small : big;

            currentParams.videoResolution = videoEncParams.videoResolution;
            currentParams.fps = videoEncParams.fps;
            currentParams.bitrate = videoEncParams.bitrate;
        }
    }

    public void reset() {
        hasMicrophonePermission.set(false);
        isMicrophoneOpened.set(false);
        isMicrophoneMuted.set(true);
        hasCameraPermission.set(false);
        isCameraOpened.set(false);
        isFrontCamera.set(true);
    }
}
