package com.tencent.effect.beautykit.provider;

import android.content.Context;
import android.util.ArrayMap;

import com.tencent.xmagic.XmagicConstant;
import com.tencent.effect.beautykit.manager.TEParamManager;
import com.tencent.effect.beautykit.model.TEUIProperty;
import com.tencent.effect.beautykit.utils.provider.ProviderUtils;

import java.util.Collections;
import java.util.List;
import java.util.Map;



public class TEGeneralDataProvider extends TEAbstractPanelDataProvider {

    private Map<TEUIProperty.UICategory, TEUIProperty> dataCategory = new ArrayMap<>();


    @Override
    public List<TEUIProperty> forceRefreshPanelData(Context context) {
        super.forceRefreshPanelData(context);
        for (TEUIProperty teuiProperty : allData) {
            dataCategory.put(teuiProperty.uiCategory, teuiProperty);
        }
        return allData;
    }

    @Override
    public List<TEUIProperty> onItemClick(TEUIProperty uiProperty) {
        switch (uiProperty.uiCategory) {
            case BEAUTY:
            case BODY_BEAUTY:
            case LUT:
                TEUIProperty currentProperty = dataCategory.get(uiProperty.uiCategory);
                if ((uiProperty.propertyList == null && uiProperty.sdkParam != null) || uiProperty.isNoneItem()) {
                    if (currentProperty != null) {
                        ProviderUtils.revertUIState(currentProperty.propertyList, uiProperty);
                        ProviderUtils.changeParamUIState(uiProperty, TEUIProperty.UIState.CHECKED_AND_IN_USE);
                    }
                }
                break;
            case MAKEUP:
            case MOTION:
            case SEGMENTATION:
                TEUIProperty makeUpProperty = dataCategory.get(TEUIProperty.UICategory.MAKEUP);
                TEUIProperty motionProperty = dataCategory.get(TEUIProperty.UICategory.MOTION);
                TEUIProperty segProperty = dataCategory.get(TEUIProperty.UICategory.SEGMENTATION);
                if ((uiProperty.propertyList == null && uiProperty.sdkParam != null) || uiProperty.isNoneItem()) {
                    if (makeUpProperty != null) {
                        ProviderUtils.revertUIState(makeUpProperty.propertyList, uiProperty);
                    }
                    if (motionProperty != null) {
                        ProviderUtils.revertUIState(motionProperty.propertyList, uiProperty);
                    }
                    if (segProperty != null) {
                        ProviderUtils.revertUIState(segProperty.propertyList, uiProperty);
                    }
                    ProviderUtils.changeParamUIState(uiProperty, TEUIProperty.UIState.CHECKED_AND_IN_USE);
                }
                break;
            default:
                break;
        }
        return uiProperty.propertyList;
    }

    @Override
    public List<TEUIProperty.TESDKParam> getRevertData(Context context) {
        List<TEUIProperty.TESDKParam> usedList = ProviderUtils.getUsedProperties(allData);
        boolean hasLut = false;
        boolean hasMotion = false;
        for (TEUIProperty.TESDKParam param : usedList) {
            if (param.effectName.equals(XmagicConstant.EffectName.EFFECT_LUT)) {
                hasLut = true;
            }
            if (param.effectName.equals(XmagicConstant.EffectName.EFFECT_MAKEUP)
                    || param.effectName.equals(XmagicConstant.EffectName.EFFECT_MOTION)
                    || param.effectName.equals(XmagicConstant.EffectName.EFFECT_SEGMENTATION)) {
                hasMotion = true;
            }
        }


        this.forceRefreshPanelData(context.getApplicationContext());

        List<TEUIProperty.TESDKParam> defaultUsedList = ProviderUtils.getUsedProperties(allData);

        for (TEUIProperty.TESDKParam param : defaultUsedList) {
            if (param.effectName.equals(XmagicConstant.EffectName.EFFECT_LUT)) {
                hasLut = false;
            }
            if (param.effectName.equals(XmagicConstant.EffectName.EFFECT_MAKEUP)
                    || param.effectName.equals(XmagicConstant.EffectName.EFFECT_MOTION)
                    || param.effectName.equals(XmagicConstant.EffectName.EFFECT_SEGMENTATION)) {
                hasMotion = false;
            }
        }

        TEParamManager paramManager = new TEParamManager();
        paramManager.putTEParams(ProviderUtils.clone0ValuedParam(usedList));
        paramManager.putTEParams(defaultUsedList);
        if (hasLut) {
            paramManager.putTEParam(ProviderUtils.createNoneItem(XmagicConstant.EffectName.EFFECT_LUT));
        }
        if (hasMotion) {
            paramManager.putTEParam(ProviderUtils.createNoneItem(XmagicConstant.EffectName.EFFECT_MOTION));
        }
        return paramManager.getParams();
    }

    @Override
    public List<TEUIProperty.TESDKParam> getCloseEffectItems(TEUIProperty uiProperty) {

        switch (uiProperty.uiCategory) {
            case BEAUTY:
            case BODY_BEAUTY:
                TEUIProperty currentProperty = dataCategory.get(uiProperty.uiCategory);
                if (currentProperty != null) {
                    List<TEUIProperty.TESDKParam> usedList =
                            ProviderUtils.getUsedProperties(currentProperty.propertyList);
                    ProviderUtils.changParamValuedTo0(usedList);
                    return usedList;
                }
                break;
            case LUT:
                return Collections.singletonList(ProviderUtils.createNoneItem(XmagicConstant.EffectName.EFFECT_LUT));
            case MAKEUP:
            case MOTION:
            case SEGMENTATION:
                return Collections.singletonList(ProviderUtils.createNoneItem(XmagicConstant.EffectName.EFFECT_MOTION));
            default:
                break;
        }
        return null;
    }
}
