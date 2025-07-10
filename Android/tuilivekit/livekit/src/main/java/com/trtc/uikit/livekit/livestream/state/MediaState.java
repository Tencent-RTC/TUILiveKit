package com.trtc.uikit.livekit.livestream.state;

import androidx.lifecycle.MutableLiveData;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;

public class MediaState {
    public MutableLiveData<Boolean> isAudioLocked       = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> isVideoLocked       = new MutableLiveData<>(false);
    public VideoAdvanceSetting      videoAdvanceSetting = new VideoAdvanceSetting();
    public VideoEncParams           videoEncParams      = new VideoEncParams();

    public void reset() {
    }

    public static class VideoAdvanceSetting {
        public boolean isVisible         = false;
        public boolean isUltimateEnabled = false;
        public boolean isH265Enabled     = false;
        public boolean isBFrameEnabled   = false;
    }

    public static class VideoEncParams {

        public TUIRoomDefine.RoomVideoEncoderParams big;
        public TUIRoomDefine.RoomVideoEncoderParams small;
        public VideoEncType                         currentEncType;

        public enum VideoEncType {
            BIG,
            SMALL
        }

        public VideoEncParams() {
            big = new TUIRoomDefine.RoomVideoEncoderParams();
            big.videoResolution = TUIRoomDefine.VideoQuality.Q_1080P;
            big.bitrate = 4000;
            big.fps = 30;
            big.resolutionMode = TUIRoomDefine.ResolutionMode.PORTRAIT;

            small = new TUIRoomDefine.RoomVideoEncoderParams();
            small.videoResolution = TUIRoomDefine.VideoQuality.Q_540P;
            small.bitrate = 1800;
            small.fps = 30;
            small.resolutionMode = TUIRoomDefine.ResolutionMode.PORTRAIT;
        }

        public TUIRoomDefine.RoomVideoEncoderParams getCurrentEnc() {
            if (currentEncType == VideoEncType.SMALL) {
                return small;
            }
            return big;
        }

        public void setCurrentEnc(TUIRoomDefine.RoomVideoEncoderParams videoEncParams) {
            TUIRoomDefine.RoomVideoEncoderParams currentParams = (currentEncType == VideoEncType.SMALL) ? small : big;
            currentParams.videoResolution = videoEncParams.videoResolution;
            currentParams.fps = videoEncParams.fps;
            currentParams.bitrate = videoEncParams.bitrate;
        }
    }
}
