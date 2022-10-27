package com.tencent.qcloud.tuikit.tuiaudioeffect.core;

import android.content.Context;

import com.tencent.liteav.audio.TXAudioEffectManager;
import com.tencent.qcloud.tuicore.interfaces.ITUIExtension;
import com.tencent.qcloud.tuikit.tuiaudioeffect.util.AudioEffectUtils;
import com.tencent.qcloud.tuikit.tuiaudioeffect.view.TUIAudioEffectButton;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * TUIAudioEffect扩展插件，插件返给调用者一个插件视图 TUIAudioEffectButton{@link TUIAudioEffectButton}
 * <p>
 * 调用者执行 TUICore.getExtensionInfo{@link com.tencent.qcloud.tuicore.TUICore#getExtensionInfo(String, Map)}来获取
 * TUIAudioEffectButton
 * eg：
 * Map<String, Object> param = new HashMap<>();
 * param.put(AudioEffectUtils.KEY_CONTEXT, context);
 * param.put(AudioEffectUtils.KEY_AUDIOEFFECTMANAGER, TRTCCloud.sharedInstance(context).getAudioEffectManager());
 * Map<String, Object> map = TUICore.getExtensionInfo(AudioEffectUtils.EXTENSION_AUDIOEFFECT, param);
 * TUIAudioEffectButton extension = (TUIAudioEffectButton) map.get(AudioEffectUtils.KEY_AUDIOEFFECTEXTENSION);
 */
public class TUIAudioEffectExtension implements ITUIExtension {

    public TUIAudioEffectExtension() {

    }

    @Override
    public Map<String, Object> onGetExtensionInfo(String key, Map<String, Object> param) {
        if (!AudioEffectUtils.EXTENSION_AUDIOEFFECT.equals(key) || null == param) {
            return Collections.EMPTY_MAP;
        }
        Context context = (Context) param.get(AudioEffectUtils.KEY_CONTEXT);
        TXAudioEffectManager effectManager = (TXAudioEffectManager) param.get(AudioEffectUtils.KEY_AUDIOEFFECTMANAGER);
        if (null == context || null == effectManager) {
            return Collections.EMPTY_MAP;
        }
        TUIAudioEffectButton extension = new TUIAudioEffectButton(context, effectManager);
        Map<String, Object> map = new HashMap<>();
        map.put(AudioEffectUtils.KEY_AUDIOEFFECTEXTENSION, extension);
        return map;
    }
}
