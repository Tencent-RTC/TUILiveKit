package com.tencent.effect.beautykit.provider;

import android.content.Context;
import android.text.TextUtils;
import android.util.ArrayMap;


import com.google.gson.Gson;
import com.tencent.xmagic.XmagicConstant;
import com.tencent.effect.beautykit.manager.TEParamManager;
import com.tencent.effect.beautykit.model.TEUIProperty;
import com.tencent.effect.beautykit.utils.provider.ProviderUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;


public class TEBeautyTemplateProvider extends TEAbstractPanelDataProvider {


    private final TEParamManager paramManager = new TEParamManager();

    private TEUIProperty currentUIProperty = null;

    private final Map<String, String> originalData = new ArrayMap<>();


    @Override
    public List<TEUIProperty> getPanelData(Context context) {
        return super.getPanelData(context);
    }

    @Override
    public List<TEUIProperty> onItemClick(TEUIProperty uiProperty) {

        List<TEUIProperty> processData = new ArrayList<>();
        for (TEUIProperty property : allData) {
            if (property.uiCategory == uiProperty.uiCategory) {
                processData.add(property);
            }
        }
        if ((uiProperty.propertyList == null && uiProperty.sdkParam != null) || uiProperty.isNoneItem()) {
            ProviderUtils.revertUIState(processData, uiProperty);
            ProviderUtils.changeParamUIState(uiProperty, TEUIProperty.UIState.CHECKED_AND_IN_USE);
        } else if (uiProperty.paramList != null || uiProperty.isNoneItem()) {
            ProviderUtils.revertUIState(processData, uiProperty);
            ProviderUtils.changeParamUIState(uiProperty, TEUIProperty.UIState.CHECKED_AND_IN_USE);
        }
        return uiProperty.propertyList;
    }


    @Override
    public List<TEUIProperty.TESDKParam> getRevertData(Context context) {
        return null;
    }

    @Override
    public List<TEUIProperty.TESDKParam> getCloseEffectItems(TEUIProperty uiProperty) {
        TEUIProperty teuiProperty = ProviderUtils.getUIPropertyByCategory(allData, uiProperty.uiCategory);
        if (teuiProperty == null) {
            return null;
        }

        if (teuiProperty.uiCategory == TEUIProperty.UICategory.MAKEUP) {
            return Collections.singletonList(ProviderUtils.createNoneItem(XmagicConstant.EffectName.EFFECT_MAKEUP));
        }
        return null;
    }



    @Override
    public List<TEUIProperty.TESDKParam> getUsedProperties() {
        for (TEUIProperty teuiProperty : allData) {
            if (teuiProperty.uiCategory == TEUIProperty.UICategory.MAKEUP) {
                return ProviderUtils.getUsedProperties(teuiProperty.propertyList);
            } else {
                return this.getBeautyUsedProperties(teuiProperty);
            }
        }
        return null;
    }


    private List<TEUIProperty.TESDKParam> getBeautyUsedProperties(TEUIProperty teuiProperty) {
        for (TEUIProperty property : teuiProperty.propertyList) {
            if (property.getUiState() == TEUIProperty.UIState.CHECKED_AND_IN_USE) {
                return this.getBeautyTemplateData(property);
            }
        }
        return null;
    }

    @Override
    public boolean isShowCompareBtn() {
        return true;
    }


    @Override
    public List<TEUIProperty.TESDKParam> getBeautyTemplateData(TEUIProperty teuiProperty) {
        this.currentUIProperty = teuiProperty;
        if (teuiProperty != null && teuiProperty.isNoneItem()) {
            this.changeParamEffectValue();
            return this.paramManager.getParams();
        } else if (teuiProperty != null) {
            this.changeParamEffectValue();
            this.putData(teuiProperty);
            this.paramManager.putTEParams(teuiProperty.paramList);
            return this.paramManager.getParams();
        }
        return null;
    }

    @Override
    public List<TEUIProperty.TESDKParam> getBeautyTemplateData() {
        return this.currentUIProperty.paramList;
    }

    public void putData(TEUIProperty teuiProperty) {
        String key = getOriginalMapKey(teuiProperty);
        String result = this.originalData.get(key);
        if (TextUtils.isEmpty(result)) {
            this.originalData.put(key, new Gson().toJson(teuiProperty.paramList));
        }
    }

    private void changeParamEffectValue() {
        List<TEUIProperty.TESDKParam> allParams = this.paramManager.getParams();
        for (TEUIProperty.TESDKParam teParam : allParams) {
            try {
                TEUIProperty.TESDKParam param = teParam.clone();
                param.effectValue = 0;
                this.paramManager.putTEParam(param);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
    }


    private String getOriginalMapKey(TEUIProperty teuiProperty) {
        return teuiProperty.displayName + teuiProperty.displayNameEn;
    }


    @Override
    public String getOriginalParam() {
        return this.originalData.get(getOriginalMapKey(this.currentUIProperty));
    }

    @Override
    public void updateBeautyTemplateData(List<TEUIProperty.TESDKParam> paramList) {
        if (this.currentUIProperty != null) {
            this.currentUIProperty.paramList = paramList;
        }
    }


}
