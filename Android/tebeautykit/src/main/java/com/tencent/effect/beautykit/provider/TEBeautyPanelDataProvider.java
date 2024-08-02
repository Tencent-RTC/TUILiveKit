package com.tencent.effect.beautykit.provider;

import android.content.Context;

import com.tencent.effect.beautykit.manager.TEParamManager;
import com.tencent.effect.beautykit.model.TEUIProperty;
import com.tencent.effect.beautykit.utils.provider.ProviderUtils;

import java.util.ArrayList;
import java.util.List;

/**
 Beauty data processing class
 Beauty, image quality adjustment, advanced beauty, body enhancement
 */
public class TEBeautyPanelDataProvider extends TEAbstractPanelDataProvider {


    private static final String TAG = TEBeautyPanelDataProvider.class.getName();

    @Override
    public List<TEUIProperty.TESDKParam> getRevertData(Context context) {
        List<TEUIProperty.TESDKParam> usedList = ProviderUtils.getUsedProperties(allData);
        this.forceRefreshPanelData(context.getApplicationContext());
        List<TEUIProperty.TESDKParam> defaultUsedList = ProviderUtils.getUsedProperties(allData);

        TEParamManager paramManager = new TEParamManager();
        paramManager.putTEParams(ProviderUtils.clone0ValuedParam(usedList));
        paramManager.putTEParams(defaultUsedList);
        return paramManager.getParams();
    }


    @Override
    public List<TEUIProperty> onItemClick(TEUIProperty uiProperty) {
        if (uiProperty == null) {
            return null;
        } else {
            List<TEUIProperty> processData = new ArrayList<>();
            for (TEUIProperty property : allData) {
                if (property.uiCategory == uiProperty.uiCategory) {
                    processData.add(property);
                }
            }
            if ((uiProperty.propertyList == null && uiProperty.sdkParam != null) || uiProperty.isNoneItem()) {
                ProviderUtils.revertUIState(processData, uiProperty);
                ProviderUtils.changeParamUIState(uiProperty, TEUIProperty.UIState.CHECKED_AND_IN_USE);
            }
            return uiProperty.propertyList;
        }
    }


    @Override
    public List<TEUIProperty.TESDKParam> getCloseEffectItems(TEUIProperty uiProperty) {
        for (TEUIProperty teuiProperty : allData) {
            if (teuiProperty == uiProperty.parentUIProperty) {
                List<TEUIProperty.TESDKParam> usedList = ProviderUtils.getUsedProperties(teuiProperty.propertyList);
                ProviderUtils.changParamValuedTo0(usedList);
                return usedList;
            }
        }
        return null;
    }


}
