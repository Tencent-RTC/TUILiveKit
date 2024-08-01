package com.tencent.effect.beautykit.enhance;

import com.tencent.xmagic.XmagicConstant;
import com.tencent.effect.beautykit.model.TEUIProperty;

public class DefaultEnhancingStrategy implements TEParamEnhancingStrategy {


    @Override
    public TEUIProperty.TESDKParam enhanceParam(TEUIProperty.TESDKParam param) {
        if (param == null) {
            return null;
        }
        switch (param.effectName) {

            case XmagicConstant.EffectName.EFFECT_LUT:
            case XmagicConstant.EffectName.EFFECT_MAKEUP:
            case XmagicConstant.EffectName.EFFECT_MOTION:
            case XmagicConstant.EffectName.EFFECT_SEGMENTATION:
            case XmagicConstant.EffectName.BODY_AUTOTHIN_BODY_STRENGTH:
            case XmagicConstant.EffectName.BODY_LEG_STRETCH:
            case XmagicConstant.EffectName.BODY_ENLARGE_CHEST_STRENGTH:
            case XmagicConstant.EffectName.BODY_SLIM_HEAD_STRENGTH:
            case XmagicConstant.EffectName.BODY_SLIM_LEG_STRENGTH:
            case XmagicConstant.EffectName.BODY_WAIST_STRENGTH:
            case XmagicConstant.EffectName.BODY_THIN_SHOULDER_STRENGTH:
                return param;
            case XmagicConstant.BeautyConstant.BEAUTY_FACE_REMOVE_WRINKLE:
            case XmagicConstant.BeautyConstant.BEAUTY_FACE_REMOVE_LAW_LINE:
            case XmagicConstant.BeautyConstant.BEAUTY_MOUTH_LIPSTICK:
            case XmagicConstant.BeautyConstant.BEAUTY_WHITEN:
            case XmagicConstant.BeautyConstant.BEAUTY_FACE_SOFTLIGHT:
            case XmagicConstant.BeautyConstant.BEAUTY_FACE_SHORT:
            case XmagicConstant.BeautyConstant.BEAUTY_FACE_V:
            case XmagicConstant.BeautyConstant.BEAUTY_EYE_DISTANCE:
            case XmagicConstant.BeautyConstant.BEAUTY_NOSE_HEIGHT:
                return changeParamValue(param, 1.3f);
            case XmagicConstant.BeautyConstant.BEAUTY_EYE_LIGHTEN:
                return changeParamValue(param, 1.5f);
            case XmagicConstant.BeautyConstant.BEAUTY_FACE_RED_CHEEK:
                return changeParamValue(param, 1.8f);
            default: {
                return changeParamValue(param, 1.2f);
            }
        }
    }

    private TEUIProperty.TESDKParam changeParamValue(TEUIProperty.TESDKParam param, float multiple) {
        TEUIProperty.TESDKParam resultParam = null;
        try {
            resultParam = param.clone();
        } catch (Exception e) {
            e.printStackTrace();
        }
        if (resultParam == null) {
            return param;
        }
        resultParam.effectValue = (int) (multiple * param.effectValue);
        return resultParam;
    }
}
