package com.tencent.effect.beautykit.manager;

import android.text.TextUtils;
import com.tencent.xmagic.XmagicConstant;
import com.tencent.effect.beautykit.model.TEUIProperty;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class TEParamManager {

    private final Map<String, TEUIProperty.TESDKParam> allData = new LinkedHashMap<>();


    public void putTEParam(TEUIProperty.TESDKParam param) {
        if (param != null && !TextUtils.isEmpty(param.effectName)) {
            if (!this.isCameraMoveItem(param)) {
                allData.put(getKey(param), param);
            }
        }
    }

    public void putTEParams(List<TEUIProperty.TESDKParam> paramList) {
        if (paramList != null && paramList.size() > 0) {
            for (TEUIProperty.TESDKParam teParam : paramList) {
                putTEParam(teParam);
            }
        }
    }

    private String getKey(TEUIProperty.TESDKParam param) {
        switch (param.effectName) {
            case XmagicConstant.EffectName.BEAUTY_WHITEN:
            case XmagicConstant.EffectName.BEAUTY_WHITEN_2:
            case XmagicConstant.EffectName.BEAUTY_WHITEN_3:
                return XmagicConstant.EffectName.BEAUTY_WHITEN;
            case XmagicConstant.EffectName.BEAUTY_FACE_NATURE:
            case XmagicConstant.EffectName.BEAUTY_FACE_GODNESS:
            case XmagicConstant.EffectName.BEAUTY_FACE_MALE_GOD:
                return XmagicConstant.EffectName.BEAUTY_FACE_NATURE;
            case XmagicConstant.EffectName.EFFECT_MAKEUP:
            case XmagicConstant.EffectName.EFFECT_SEGMENTATION:
            case XmagicConstant.EffectName.EFFECT_MOTION:
                return XmagicConstant.EffectName.EFFECT_MOTION;
            default:
                return param.effectName;
        }
    }

    private boolean isCameraMoveItem(TEUIProperty.TESDKParam param) {
        if (!XmagicConstant.EffectName.EFFECT_MOTION.equals(param.effectName)) {
            return false;
        }
        String resPath = param.resourcePath;
        boolean mergeWithCurrentMotion = false;
        if (param.extraInfo != null) {
            String merge = param.extraInfo.get("mergeWithCurrentMotion");
            if ("true".equals(merge)) {
                mergeWithCurrentMotion = true;
            }
        }
        return resPath != null && resPath.contains("video_camera_move_") && mergeWithCurrentMotion;
    }

    public List<TEUIProperty.TESDKParam> getParams() {
        return new ArrayList<>(allData.values());
    }

    public void clear() {
        this.allData.clear();
    }

    public boolean isEmpty() {
        return this.allData.isEmpty();
    }

}
