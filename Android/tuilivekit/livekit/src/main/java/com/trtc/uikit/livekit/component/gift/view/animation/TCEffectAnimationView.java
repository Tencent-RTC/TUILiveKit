package com.trtc.uikit.livekit.component.gift.view.animation;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;

import android.content.Context;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.util.Log;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUIService;
import com.tencent.qcloud.tuicore.interfaces.TUIExtensionInfo;
import com.tencent.qcloud.tuicore.interfaces.TUIServiceCallback;
import com.trtc.uikit.livekit.common.DataReporter;
import com.trtc.uikit.livekit.component.gift.service.GiftConstants;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public final class TCEffectAnimationView extends AnimationView {

    private static final String TAG = "EffectAnimationView";

    private static final String KEY_EXTENSION_NAME         = "TUIEffectPlayerExtension";
    private static final String KEY_SERVICE_NAME           = "TUIEffectPlayerService";
    private static final String KEY_GET_VIEW               = "TCEffectAnimView";
    private static final String KEY_METHOD_START_PLAY      = "startPlay";
    private static final String KEY_METHOD_STOP_PLAY       = "stopPlay";
    private static final String KEY_PARAM_PLAY_URL         = "playUrl";
    private static final String KEY_PARAM_CONTEXT          = "context";
    private static final String KEY_PARAM_VIEW             = "view";
    private static final String KEY_PARAM_CLEAR_LAST_FRAME = "clearLastFrame";

    private final View               mEffectAnimView;
    private final ITUIService        mEffectPlayerService;
    private final TUIServiceCallback mServiceCallback = new TUIServiceCallback() {
        @Override
        public void onServiceCallback(int errorCode, String errorMessage, Bundle bundle) {
            if (mCallback != null) {
                mCallback.onFinished(errorCode);
            }
        }
    };

    public TCEffectAnimationView(@NonNull Context context) {
        this(context, null);
    }

    public TCEffectAnimationView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        mEffectAnimView = createAnimationView();
        if (mEffectAnimView != null) {
            mEffectPlayerService = TUICore.getService(KEY_SERVICE_NAME);
            LayoutParams params = new LayoutParams(MATCH_PARENT, MATCH_PARENT);
            addView(mEffectAnimView, params);
        } else {
            mEffectPlayerService = null;
        }
    }

    private View createAnimationView() {
        Map<String, Object> map = new HashMap<>();
        map.put(KEY_PARAM_CONTEXT, getContext());
        map.put(KEY_GET_VIEW, "");
        List<TUIExtensionInfo> list = TUICore.getExtensionList(KEY_EXTENSION_NAME, map);
        if (!list.isEmpty()) {
            TUIExtensionInfo info = list.get(0);
            Map<String, Object> data = info.getData();
            if (data != null && data.containsKey(KEY_GET_VIEW)) {
                Object view = data.get(KEY_GET_VIEW);
                return (View) view;
            }
        }
        Log.w(TAG, "TUIEffectPlayerExtension create view failed!");
        return null;
    }

    @Override
    public void playAnimation(String playUrl) {
        mEffectAnimView.setVisibility(VISIBLE);
        Map<String, Object> map = new HashMap<>();
        map.put(KEY_PARAM_PLAY_URL, playUrl);
        map.put(KEY_PARAM_VIEW, mEffectAnimView);
        mEffectPlayerService.onCall(KEY_METHOD_START_PLAY, map, mServiceCallback);
        reportData();
    }

    @Override
    public void stopPlay() {
        Map<String, Object> map = new HashMap<>();
        map.put(KEY_PARAM_CLEAR_LAST_FRAME, true);
        map.put(KEY_PARAM_VIEW, mEffectAnimView);
        mEffectPlayerService.onCall(KEY_METHOD_STOP_PLAY, map, mServiceCallback);
    }

    private void reportData() {
        boolean isVoiceRoom = !TextUtils.isEmpty(mRoomId) && mRoomId.startsWith("voice_");
        int key = GiftConstants.DATA_REPORT_LIVE_GIFT_EFFECT_PLAY_COUNT;
        if (isVoiceRoom) {
            key = GiftConstants.DATA_REPORT_VOICE_GIFT_EFFECT_PLAY_COUNT;
        }
        DataReporter.reportEventData(key);
    }
}
