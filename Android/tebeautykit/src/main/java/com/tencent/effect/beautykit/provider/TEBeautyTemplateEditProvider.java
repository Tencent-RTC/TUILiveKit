package com.tencent.effect.beautykit.provider;

import android.content.Context;


import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import com.tencent.effect.beautykit.manager.TEParamManager;
import com.tencent.effect.beautykit.model.TEUIProperty;
import com.tencent.effect.beautykit.utils.provider.ProviderUtils;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;


public class TEBeautyTemplateEditProvider extends TEAbstractPanelDataProvider {


    private TEParamManager teParamManager = new TEParamManager();


    private String originalParamData = null;

    public void setOriginalParamData(String originalParamData) {
        this.originalParamData = originalParamData;
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


    /**
     * The restoration logic here is as follows:
     *      Get all the currently selected data on the panel, which is the data for the current beauty effect.
     *      Adjust the values of all the data to 0 and add them to teParamManager.
     *      Parse the beauty effect data to be restored.
     *      Set this data to the provider's originalParamList variable for UI restoration.
     *      Write this data to teParamManager.
     *      Refresh the data using the getNewPanelData method.
     *      Return the data for restoring the beauty effect using the getParams() method of teParamManager.
     *      @param context Application context
     @return
     */
    @Override
    public List<TEUIProperty.TESDKParam> getRevertData(Context context) {
        this.teParamManager.clear();
        // Get all the beauty properties being used.
        List<TEUIProperty.TESDKParam> list = this.getUsedProperties();
        // Modify the values of the data to 0.
        for (TEUIProperty.TESDKParam teParam : list) {
            teParam.effectValue = 0;
            this.teParamManager.putTEParam(teParam);
        }
        Type type = new TypeToken<List<TEUIProperty.TESDKParam>>() {
        }.getType();
        this.originalParamList = new Gson().fromJson(this.originalParamData, type);
        this.teParamManager.putTEParams(this.originalParamList);
        this.forceRefreshPanelData(context);
        return this.teParamManager.getParams();
    }


    @Override
    public List<TEUIProperty.TESDKParam> getCloseEffectItems(TEUIProperty uiProperty) {
        for (TEUIProperty teuiProperty : allData) {
            if (teuiProperty == uiProperty) {
                List<TEUIProperty.TESDKParam> usedList = ProviderUtils.getUsedProperties(teuiProperty.propertyList);
                ProviderUtils.changParamValuedTo0(usedList);
                return usedList;
            }
        }
        return null;
    }


    @Override
    public List<TEUIProperty.TESDKParam> getUsedProperties() {
        List<TEUIProperty.TESDKParam> properties = ProviderUtils.getUsedProperties(allData);
        List<TEUIProperty.TESDKParam> resultList = new ArrayList<>();
        for (TEUIProperty.TESDKParam param : properties) {
            try {
                resultList.add(param.clone());
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return resultList;
    }

    @Override
    public boolean isShowCompareBtn() {
        return true;
    }


}
