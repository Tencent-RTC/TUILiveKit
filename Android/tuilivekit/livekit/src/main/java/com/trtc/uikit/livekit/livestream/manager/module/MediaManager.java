package com.trtc.uikit.livekit.livestream.manager.module;

import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.VideoQuality.Q_1080P;

import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUICore;
import com.trtc.uikit.livekit.livestream.manager.api.ILiveService;
import com.trtc.uikit.livekit.livestream.manager.api.LiveStreamLog;
import com.trtc.uikit.livekit.livestream.state.LiveState;
import com.trtc.uikit.livekit.livestream.state.MediaState;
import com.trtc.uikit.livekit.livestream.view.widgets.beauty.TEBeautyManager;

import java.util.HashMap;
import java.util.Map;

public class MediaManager extends BaseManager {
    private static final String TAG = "MediaManager";

    public MediaManager(LiveState state, ILiveService service) {
        super(state, service);
        enableUltimate(true);
        enableH265(true);
        initBeautyConfig();
    }

    @Override
    public void destroy() {
        LiveStreamLog.info(TAG + " destroy");
        enableUltimate(false);
        enableH265(false);
        resetVideoQualityEx();
        closeBeauty();
    }

    public void setLocalVideoView(TUIVideoView view) {
        mLiveService.setLocalVideoView(view);
    }

    public void enableAdvancedVisible(boolean visible) {
        mMediaState.videoAdvanceSetting.isVisible = visible;
    }

    public void enableUltimate(boolean enable) {
        mMediaState.videoAdvanceSetting.isUltimateEnabled = enable;
        Map<String, Object> params = new HashMap<>();
        params.put("enable", enable);
        TUICore.callService("VideoAdvanceExtension", "enableUltimate", params);

        if (enable) {
            enableBFrame(false);
        }
    }

    public void enableBFrame(boolean enable) {
        mMediaState.videoAdvanceSetting.isBFrameEnabled = enable;
        Map<String, Object> params = new HashMap<>();
        params.put("enable", enable);
        TUICore.callService("VideoAdvanceExtension", "enableBFrame", params);
    }

    public void enableH265(boolean enable) {
        mMediaState.videoAdvanceSetting.isH265Enabled = enable;
        Map<String, Object> params = new HashMap<>();
        params.put("enable", enable);
        TUICore.callService("VideoAdvanceExtension", "enableH265", params);
    }

    public void updateVideoQualityEx(TUIRoomDefine.RoomVideoEncoderParams videoEncParams) {
        mMediaState.videoEncParams.setCurrentEnc(videoEncParams);
        mLiveService.updateVideoQualityEx(videoEncParams);
    }

    public void changeVideoEncParams(MediaState.VideoEncParams.VideoEncType encType) {
        LiveStreamLog.info(TAG + " changeVideoEncParams:[" + encType + "]");
        mMediaState.videoEncParams.currentEncType = encType;
        if (encType == MediaState.VideoEncParams.VideoEncType.BIG) {
            updateVideoQualityEx(mMediaState.videoEncParams.big);
        } else if (encType == MediaState.VideoEncParams.VideoEncType.SMALL) {
            updateVideoQualityEx(mMediaState.videoEncParams.small);
        }
    }

    public void setBeautyLevel(int level) {
        mLiveService.setBeautyLevel(level);
        mBeautyState.smoothLevel.setValue(level);
    }

    public void setWhitenessLevel(int level) {
        mLiveService.setWhitenessLevel(level);
        mBeautyState.whitenessLevel.setValue(level);
    }

    public void setRuddyLevel(int level) {
        mLiveService.setRuddyLevel(level);
        mBeautyState.ruddyLevel.setValue(level);
    }

    public void closeBeauty() {
        setBeautyLevel(0);
        setWhitenessLevel(0);
        setRuddyLevel(0);
    }

    public void setCustomVideoProcess() {
        TEBeautyManager.getInstance().setCustomVideoProcess();
    }

    private void initBeautyConfig() {
        setBeautyLevel(mBeautyState.smoothLevel.getValue());
        setWhitenessLevel(mBeautyState.whitenessLevel.getValue());
        setRuddyLevel(mBeautyState.ruddyLevel.getValue());
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
