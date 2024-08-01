package com.tencent.effect.beautykit.provider;

import android.content.Context;


import com.tencent.effect.beautykit.model.TEPanelDataModel;
import com.tencent.effect.beautykit.model.TEUIProperty;

import java.util.List;

public interface TEPanelDataProvider {
    void setPanelDataList(List<TEPanelDataModel> panelDataList);

    void setUsedParams(List<TEUIProperty.TESDKParam> paramList);

    List<TEUIProperty> getPanelData(Context context);

    List<TEUIProperty> forceRefreshPanelData(Context context);

    void onTabItemClick(int index);

    List<TEUIProperty> onItemClick(TEUIProperty uiProperty);

    List<TEUIProperty.TESDKParam> getRevertData(Context context);

    List<TEUIProperty.TESDKParam> getCloseEffectItems(TEUIProperty uiProperty);

    List<TEUIProperty.TESDKParam> getUsedProperties();

    boolean isShowCompareBtn();

    String getOriginalParam();

    void unCheckAll();

    void putMutuallyExclusiveProvider(List<TEPanelDataProvider> providerList);

    List<TEUIProperty.TESDKParam> getBeautyTemplateData(TEUIProperty uiProperty);

    List<TEUIProperty.TESDKParam> getBeautyTemplateData();

    void updateBeautyTemplateData(List<TEUIProperty.TESDKParam> paramList);

}
