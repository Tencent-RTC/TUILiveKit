package com.tencent.tcmediax.tceffectplayerkit.core;

import android.content.Context;

import com.tencent.qcloud.tuicore.interfaces.ITUIExtension;
import com.tencent.qcloud.tuicore.interfaces.TUIExtensionInfo;
import com.tencent.tcmediax.tceffectplayerkit.Constants;
import com.tencent.tcmediax.tceffectplayer.api.TCEffectAnimView;
import com.tencent.tcmediax.tceffectplayer.api.TCEffectPlayerConstant;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TUIEffectPlayerExtension implements ITUIExtension {

    @Override
    public List<TUIExtensionInfo> onGetExtension(String extensionID, Map<String, Object> param) {
        HashMap<String, Object> hashMap = new HashMap<>();
        if (param != null && Constants.KEY_EXTENSION_NAME.equals(extensionID)) {
            Context context = (Context) param.get(Constants.KEY_PARAM_CONTEXT);
            if (param.containsKey(Constants.KEY_GET_VIEW)) {
                TCEffectAnimView animView = new TCEffectAnimView(context);
                animView.setScaleType(TCEffectPlayerConstant.ScaleType.FIT_XY);
                hashMap.put(Constants.KEY_GET_VIEW, animView);
                TUIExtensionInfo info = new TUIExtensionInfo();
                info.setData(hashMap);
                return Collections.singletonList(info);
            }
        }
        return null;
    }
}
