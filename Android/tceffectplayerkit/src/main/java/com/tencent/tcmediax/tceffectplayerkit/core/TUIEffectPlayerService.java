package com.tencent.tcmediax.tceffectplayerkit.core;

import android.content.Context;
import android.os.Bundle;
import android.text.TextUtils;

import com.tencent.qcloud.tuicore.interfaces.ITUIService;
import com.tencent.qcloud.tuicore.interfaces.TUIServiceCallback;
import com.tencent.tcmediax.api.TCMediaXBase;
import com.tencent.tcmediax.tceffectplayerkit.Constants;
import com.tencent.tcmediax.tceffectplayer.api.TCEffectAnimView;

import java.util.Map;

public class TUIEffectPlayerService implements ITUIService {

    @Override
    public Object onCall(String method, Map<String, Object> param, TUIServiceCallback callback) {
        if (param != null && TextUtils.equals(Constants.KEY_METHOD_START_PLAY, method)) {
            String url = (String) param.get(Constants.KEY_PARAM_PLAY_URL);
            Object view = param.get(Constants.KEY_PARAM_VIEW);
            if (view instanceof TCEffectAnimView) {
                TCEffectAnimView animView = (TCEffectAnimView) view;
                animView.setPlayListener(new AnimPlayListener(callback));
                animView.startPlay(url);
            }
        } else if (param != null && TextUtils.equals(Constants.KEY_METHOD_STOP_PLAY, method)) {
            Object view = param.get(Constants.KEY_PARAM_VIEW);
            Boolean clearLastFrame = (Boolean) param.get(Constants.KEY_PARAM_CLEAR_LAST_FRAME);
            if (view instanceof TCEffectAnimView) {
                TCEffectAnimView animView = (TCEffectAnimView) view;
                animView.setPlayListener(null);
                animView.stopPlay(Boolean.TRUE.equals(clearLastFrame));
            }
        } else if (param != null && TextUtils.equals(Constants.KEY_METHOD_SET_LICENSE, method)) {
            String url = (String) param.get(Constants.KEY_PARAM_LICENSE_URL);
            String key = (String) param.get(Constants.KEY_PARAM_LICENSE_KEY);
            Context context = (Context) param.get(Constants.KEY_PARAM_CONTEXT);
            TCMediaXBase.getInstance().setLicense(context, url, key, (i, s) -> {
                if (callback != null) {
                    callback.onServiceCallback(i, s, null);
                }
            });
        }
        return null;
    }

    private static final class AnimPlayListener implements TCEffectAnimView.IAnimPlayListener {

        private final TUIServiceCallback mCallback;

        public AnimPlayListener(TUIServiceCallback callback) {
            mCallback = callback;
        }

        @Override
        public void onPlayStart() {

        }

        @Override
        public void onPlayEnd() {
            if (mCallback != null) {
                mCallback.onServiceCallback(0, "", null);
            }
        }

        @Override
        public void onPlayError(int i) {
            if (mCallback != null) {
                mCallback.onServiceCallback(i, "", null);
            }
        }

        @Override
        public void onPlayEvent(int i, Bundle bundle) {

        }
    }
}
