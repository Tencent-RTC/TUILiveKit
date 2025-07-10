package com.trtc.uikit.livekit.livestream.manager.module;

import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.VideoQuality.Q_1080P;

import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUICore;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.beauty.tebeauty.TEBeautyManager;
import com.trtc.uikit.livekit.livestream.manager.api.ILiveService;
import com.trtc.uikit.livekit.livestream.state.LiveState;
import com.trtc.uikit.livekit.livestream.state.MediaState;

import java.util.HashMap;
import java.util.Map;

public class MediaManager extends BaseManager {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("MediaManager");

    public MediaManager(LiveState state, ILiveService service) {
        super(state, service);
        enableUltimate(true);
        enableH265(true);
    }

    @Override
    public void destroy() {
        LOGGER.info("destroy");
        enableUltimate(false);
        enableH265(false);
        resetVideoQualityEx();
    }

    public void setLocalVideoView(TUIVideoView view) {
        mLiveService.setLocalVideoView(view);
    }

    public void enableUltimate(boolean enable) {
        mMediaState.videoAdvanceSetting.isUltimateEnabled = enable;
        Map<String, Object> params = new HashMap<>();
        params.put("enable", enable);
        TUICore.callService("AdvanceSettingManager", "enableUltimate", params);

        if (enable) {
            enableBFrame(false);
        }
    }

    public void enableBFrame(boolean enable) {
        mMediaState.videoAdvanceSetting.isBFrameEnabled = enable;
        Map<String, Object> params = new HashMap<>();
        params.put("enable", enable);
        TUICore.callService("AdvanceSettingManager", "enableBFrame", params);
    }

    public void enableH265(boolean enable) {
        mMediaState.videoAdvanceSetting.isH265Enabled = enable;
        Map<String, Object> params = new HashMap<>();
        params.put("enable", enable);
        TUICore.callService("AdvanceSettingManager", "enableH265", params);
    }

    public void updateVideoQualityEx(TUIRoomDefine.RoomVideoEncoderParams videoEncParams) {
        mMediaState.videoEncParams.setCurrentEnc(videoEncParams);
        mLiveService.updateVideoQualityEx(videoEncParams);
    }

    public void changeVideoEncParams(MediaState.VideoEncParams.VideoEncType encType) {
        LOGGER.info("changeVideoEncParams:[" + encType + "]");
        mMediaState.videoEncParams.currentEncType = encType;
        if (encType == MediaState.VideoEncParams.VideoEncType.BIG) {
            updateVideoQualityEx(mMediaState.videoEncParams.big);
        } else if (encType == MediaState.VideoEncParams.VideoEncType.SMALL) {
            updateVideoQualityEx(mMediaState.videoEncParams.small);
        }
    }

    public void setCustomVideoProcess() {
        TEBeautyManager.getInstance().setCustomVideoProcess();
    }

    private void resetVideoQualityEx() {
        TUIRoomDefine.RoomVideoEncoderParams roomVideoEncoderParams = new TUIRoomDefine.RoomVideoEncoderParams();
        roomVideoEncoderParams.videoResolution = Q_1080P;
        roomVideoEncoderParams.bitrate = 4000;
        roomVideoEncoderParams.fps = 30;
        roomVideoEncoderParams.resolutionMode = TUIRoomDefine.ResolutionMode.PORTRAIT;
        mLiveService.updateVideoQualityEx(roomVideoEncoderParams);
    }
}
