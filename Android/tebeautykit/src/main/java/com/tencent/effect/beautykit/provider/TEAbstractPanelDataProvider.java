package com.tencent.effect.beautykit.provider;

import android.content.Context;
import android.text.TextUtils;
import android.util.ArrayMap;

import com.google.gson.Gson;
import com.tencent.effect.beautykit.model.TEPanelDataModel;
import com.tencent.effect.beautykit.model.TEUIProperty;
import com.tencent.xmagic.XmagicConstant;
import com.tencent.effect.beautykit.utils.provider.ProviderUtils;
import com.tencent.xmagic.util.FileUtil;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;


abstract class TEAbstractPanelDataProvider implements TEPanelDataProvider {

    private static final String TAG = TEAbstractPanelDataProvider.class.getName();

    protected Context applicationContext = null;
    protected List<TEUIProperty> allData = null;


    /**
     * Used to store the mapping between TEParam.effectName and TEUIProperty.
     */
    private final Map<String, TEUIProperty> uiPropertyIndexByNameMap = new ArrayMap<>();

    private List<TEPanelDataModel> panelDataModels = null;


    @Override
    public void setPanelDataList(List<TEPanelDataModel> dataModels) {
        this.panelDataModels = dataModels;
    }

    /**
     *  Used to store the incoming data, which is the original data.
     *  This data is mainly used to restore the selected state of UI on the panel to the previous state.
     */
    protected List<TEUIProperty.TESDKParam> originalParamList = null;

    @Override
    public void setUsedParams(List<TEUIProperty.TESDKParam> paramList) {
        this.originalParamList = paramList;
    }

    @Override
    public List<TEUIProperty> getPanelData(Context context) {
        if (allData != null) {
            return allData;
        }
        return this.forceRefreshPanelData(context);
    }

    @Override
    public List<TEUIProperty> forceRefreshPanelData(Context context) {
        this.applicationContext = context.getApplicationContext();
        if (allData == null) {
            allData = new ArrayList<>();
        } else {
            allData.clear();
        }
        uiPropertyIndexByNameMap.clear();
        for (TEPanelDataModel dataModel : this.panelDataModels) {
            if (TextUtils.isEmpty(dataModel.jsonFilePath)) {
                continue;
            }
            String dataStr = null;
            if (dataModel.jsonFilePath.startsWith(File.separator)) {
                dataStr = FileUtil.readFile(dataModel.jsonFilePath);
            } else {
                dataStr = FileUtil.readAssetFile(context, dataModel.jsonFilePath);
            }
            if (TextUtils.isEmpty(dataStr)) {
                continue;
            }
            TEUIProperty uiProperty = new Gson().fromJson(dataStr.trim(), TEUIProperty.class);
            uiProperty.uiCategory = dataModel.category;
            this.completionParam(uiProperty.propertyList, dataModel.category, uiProperty);
            this.putDataToMap(uiProperty);
            this.allData.add(uiProperty);
        }
        this.syncUIState();
        return allData;
    }


    /**
     * Synchronize UI state.
     */
    private void syncUIState() {
        if (this.originalParamList == null || this.originalParamList.size() == 0) {
            return;
        }
        for (TEUIProperty.TESDKParam param : this.originalParamList) {
            // Assuming this data is for beauty or body enhancement, we can query it using the effectName.
            TEUIProperty teuiProperty = uiPropertyIndexByNameMap.get(this.getNameMapKey(param));
            if (teuiProperty != null) {
                teuiProperty.sdkParam.effectValue = param.effectValue;
                if (teuiProperty.uiCategory == TEUIProperty.UICategory.BEAUTY || teuiProperty.uiCategory
                        == TEUIProperty.UICategory.BODY_BEAUTY) {
                    teuiProperty.setUiState(TEUIProperty.UIState.IN_USE);
                    ProviderUtils.changeParentUIState(teuiProperty, TEUIProperty.UIState.IN_USE);
                } else {
                    if (teuiProperty.uiCategory == TEUIProperty.UICategory.SEGMENTATION && param.extraInfo != null) {
                        teuiProperty.sdkParam.extraInfo = param.extraInfo;
                    }
                    teuiProperty.setUiState(TEUIProperty.UIState.CHECKED_AND_IN_USE);
                    ProviderUtils.changeParentUIState(teuiProperty, TEUIProperty.UIState.CHECKED_AND_IN_USE);
                }
            }
        }
        List<TEUIProperty> allBeautyPropertyList = new ArrayList<>();
        for (TEUIProperty teuiProperty : allData) {
            if (teuiProperty.uiCategory == TEUIProperty.UICategory.BODY_BEAUTY
                    && teuiProperty.propertyList != null) {
                ProviderUtils.findFirstInUseItemAndMakeChecked(teuiProperty.propertyList);
            }
            if (teuiProperty.uiCategory == TEUIProperty.UICategory.BEAUTY) {
                allBeautyPropertyList.add(teuiProperty);
            }
        }
        if (allBeautyPropertyList.size() > 0) {
            ProviderUtils.findFirstInUseItemAndMakeChecked(allBeautyPropertyList);
        }

    }









    private void putDataToMap(TEUIProperty property) {
        if (this.originalParamList == null || this.originalParamList.size() == 0) {
            return;
        }
        property.setUiState(TEUIProperty.UIState.INIT);
        if (property.sdkParam != null) {
            uiPropertyIndexByNameMap.put(getNameMapKey(property.sdkParam), property);
        }
    }


    @Override
    public void onTabItemClick(int index) {
        if (index < 0 || index >= allData.size()) {
            return;
        }
        for (int i = 0; i < allData.size(); i++) {
            TEUIProperty item = allData.get(i);
            if (i == index) {
                item.setUiState(TEUIProperty.UIState.CHECKED_AND_IN_USE);
            } else {
                item.setUiState(TEUIProperty.UIState.INIT);
            }
        }
    }

    @Override
    public abstract List<TEUIProperty> onItemClick(TEUIProperty uiProperty);


    @Override
    public abstract List<TEUIProperty.TESDKParam> getRevertData(Context context);

    @Override
    public abstract List<TEUIProperty.TESDKParam> getCloseEffectItems(TEUIProperty uiProperty);



    @Override
    public List<TEUIProperty.TESDKParam> getUsedProperties() {
        return ProviderUtils.getUsedProperties(allData);
    }

    @Override
    public boolean isShowCompareBtn() {
        return true;
    }


    @Override
    public List<TEUIProperty.TESDKParam> getBeautyTemplateData(TEUIProperty teuiProperty) {
        return null;
    }

    @Override
    public List<TEUIProperty.TESDKParam> getBeautyTemplateData() {
        return null;
    }


    @Override
    public String getOriginalParam() {
        return null;
    }

    @Override
    public void updateBeautyTemplateData(List<TEUIProperty.TESDKParam> paramList) {

    }

    @Override
    public void putMutuallyExclusiveProvider(List<TEPanelDataProvider> providerList) {

    }

    @Override
    public void unCheckAll() {

    }

    private String getNameMapKey(TEUIProperty.TESDKParam param) {
        StringBuilder keyBuilder = new StringBuilder();
        if (!TextUtils.isEmpty(param.effectName)) {
            keyBuilder.append(param.effectName);
        }
        if (!TextUtils.isEmpty(param.resourcePath)) {
            keyBuilder.append(param.resourcePath);
        }
        return keyBuilder.toString();
    }

    private void completionParam(List<TEUIProperty> list,
                                 TEUIProperty.UICategory category,
                                 TEUIProperty parentProperty) {
        for (TEUIProperty property : list) {
            property.parentUIProperty = parentProperty;
            property.uiCategory = category;
            ProviderUtils.createDlModelAndSDKParam(property, category);
            if (property.sdkParam != null) {
                switch (category) {
                    case LUT:
                        property.sdkParam.effectName = XmagicConstant.EffectName.EFFECT_LUT;
                        break;
                    case MAKEUP:
                        property.sdkParam.effectName = XmagicConstant.EffectName.EFFECT_MAKEUP;
                        break;
                    case MOTION:
                        property.sdkParam.effectName = XmagicConstant.EffectName.EFFECT_MOTION;
                        break;
                    case SEGMENTATION:
                        property.sdkParam.effectName = XmagicConstant.EffectName.EFFECT_SEGMENTATION;
                        break;
                    default:
                        break;
                }
            }
            ProviderUtils.completionResPath(property);
            this.putDataToMap(property);
            if (property.propertyList != null) {
                this.completionParam(property.propertyList, category, property);
            }
        }
    }


}
