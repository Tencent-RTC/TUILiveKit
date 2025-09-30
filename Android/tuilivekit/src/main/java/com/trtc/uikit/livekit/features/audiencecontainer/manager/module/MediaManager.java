package com.trtc.uikit.livekit.features.audiencecontainer.manager.module;

import static com.trtc.uikit.livekit.common.MutableLiveDataUtils.setValue;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.text.TextUtils;
import android.util.TypedValue;

import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.qcloud.tuicore.TUICore;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.beauty.tebeauty.TEBeautyManager;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.api.ILiveService;
import com.trtc.uikit.livekit.features.audiencecontainer.state.AudienceState;
import com.trtc.uikit.livekit.features.audiencecontainer.state.CoGuestState;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MediaManager extends BaseManager {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("MediaManager");

    public MediaManager(AudienceState state, ILiveService service) {
        super(state, service);
        enableSwitchPlaybackQuality(true);
    }

    @Override
    public void destroy() {
        LOGGER.info("destroy");
        enableSwitchPlaybackQuality(false);
    }

    public void createVideoMuteBitmap(Context context, int bigResId, int smallResId) {
        if (mMediaState.bigMuteBitmap == null)
            mMediaState.bigMuteBitmap = createMuteBitmap(context, bigResId);
        if (mMediaState.smallMuteBitmap == null)
            mMediaState.smallMuteBitmap = createMuteBitmap(context, smallResId);
    }

    public void releaseVideoMuteBitmap() {
        if (mMediaState.bigMuteBitmap != null && !mMediaState.bigMuteBitmap.isRecycled())
            mMediaState.bigMuteBitmap.recycle();
        if (mMediaState.smallMuteBitmap != null && !mMediaState.smallMuteBitmap.isRecycled())
            mMediaState.smallMuteBitmap.recycle();
        mMediaState.bigMuteBitmap = mMediaState.smallMuteBitmap = null;
    }

    private Bitmap createMuteBitmap(Context context, int resId) {
        TypedValue tv = new TypedValue();
        context.getResources().openRawResource(resId, tv);
        BitmapFactory.Options opt = new BitmapFactory.Options();
        opt.inDensity = tv.density;
        opt.inScaled = false;
        return BitmapFactory.decodeResource(context.getResources(), resId, opt);
    }

    public void setLocalVideoView(TUIVideoView view) {
        mLiveService.setLocalVideoView(view);
    }

    public void setCustomVideoProcess() {
        TEBeautyManager.INSTANCE.setCustomVideoProcess();
    }

    public void enablePictureInPictureMode(boolean enable) {
        LOGGER.info("enablePictureInPictureMode enable:" + enable);
        setValue(mMediaState.isPictureInPictureMode, enable);
    }

    public void onUserVideoSizeChanged(String roomId, String userId,
                                       TUIRoomDefine.VideoStreamType streamType,
                                       int width, int height) {
        TUIRoomDefine.VideoQuality playbackQuality = getVideoQuality(width, height);
        if (playbackQuality == mMediaState.playbackQuality.getValue()) {
            return;
        }
        if (mMediaState.playbackQualityList.getValue().size() <= 1) {
            return;
        }
        if (!mMediaState.playbackQualityList.getValue().contains(playbackQuality)) {
            return;
        }
        if (mCoGuestState.coGuestStatus.getValue() != CoGuestState.CoGuestStatus.NONE) {
            return;
        }
        mMediaState.playbackQuality.setValue(playbackQuality);
    }

    public void enableSwitchPlaybackQuality(boolean enable) {
        Map<String, Object> params = new HashMap<>();
        params.put("enable", enable);
        TUICore.callService("AdvanceSettingManager", "enableSwitchPlaybackQuality", params);
    }

    public void getMultiPlaybackQuality(String roomId) {
        String jsonStr = "{\"api\":\"queryPlaybackQualityList\", \"params\":{\"roomId\": \"" + roomId + "\"}}";
        TUIRoomEngine.sharedInstance().callExperimentalAPI(jsonStr, new TUIRoomDefine.ExperimentalAPIResponseCallback() {
            @Override
            public void onResponse(String jsonData) {
                if (TextUtils.isEmpty(jsonData)) {
                    return;
                }
                parseQualityJsonString(jsonData);
            }
        });
    }

    public void switchPlaybackQuality(TUIRoomDefine.VideoQuality quality) {
        try {
            JSONObject params = new JSONObject();
            params.put("quality", quality.getValue());
            params.put("autoSwitch", false);
            JSONObject jsonObject = new JSONObject();
            jsonObject.put("api", "switchPlaybackQuality");
            jsonObject.put("params", params);
            TUIRoomEngine.sharedInstance().callExperimentalAPI(jsonObject.toString(), null);
        } catch (JSONException e) {
            LOGGER.error("Failed to build JSON for switchPlaybackQuality: " + e.getMessage());
        }
    }

    public void parseQualityJsonString(String jsonString) {
        try {
            JSONObject jsonObject = new JSONObject(jsonString);
            JSONArray dataArray = jsonObject.getJSONArray("data");

            List<TUIRoomDefine.VideoQuality> qualityList = new ArrayList<>();
            for (int i = 0; i < dataArray.length(); i++) {
                int quality = dataArray.getInt(i);
                if (quality == TUIRoomDefine.VideoQuality.Q_1080P.getValue()) {
                    qualityList.add(TUIRoomDefine.VideoQuality.Q_1080P);
                }
                if (quality == TUIRoomDefine.VideoQuality.Q_720P.getValue()) {
                    qualityList.add(TUIRoomDefine.VideoQuality.Q_720P);
                }
                if (quality == TUIRoomDefine.VideoQuality.Q_540P.getValue()) {
                    qualityList.add(TUIRoomDefine.VideoQuality.Q_540P);
                }
                if (quality == TUIRoomDefine.VideoQuality.Q_360P.getValue()) {
                    qualityList.add(TUIRoomDefine.VideoQuality.Q_360P);
                }
            }
            mMediaState.playbackQualityList.setValue(qualityList);
            if (!qualityList.isEmpty()) {
                mMediaState.playbackQuality.setValue(qualityList.get(0));
            }
        } catch (JSONException e) {
            LOGGER.error("Failed to decode JSON: " + jsonString + "," + e.getMessage());
        }
    }

    private TUIRoomDefine.VideoQuality getVideoQuality(int width, int height) {
        int resolution = width * height;
        if (resolution <= (360 * 640)) {
            return TUIRoomDefine.VideoQuality.Q_360P;
        }
        if (resolution <= (540 * 960)) {
            return TUIRoomDefine.VideoQuality.Q_540P;
        }
        if (resolution <= (720 * 1280)) {
            return TUIRoomDefine.VideoQuality.Q_720P;
        }
        return TUIRoomDefine.VideoQuality.Q_1080P;
    }
}
