package com.tencent.effect.beautykit.provider;

import android.content.Context;


import com.tencent.xmagic.XmagicConstant;
import com.tencent.effect.beautykit.model.TEUIProperty;
import com.tencent.effect.beautykit.utils.provider.ProviderUtils;

import java.util.Collections;
import java.util.List;


public class TEMotionPanelDataProvider extends TEAbstractPanelDataProvider {

    private List<TEPanelDataProvider> dependentProviderList = null;

    @Override
    public List<TEUIProperty> onItemClick(TEUIProperty uiProperty) {
        if (uiProperty == null) {
            return null;
        }
        if ((uiProperty.propertyList == null && uiProperty.sdkParam != null) || uiProperty.isNoneItem()) {
            ProviderUtils.revertUIState(allData, uiProperty);
            ProviderUtils.changeParamUIState(uiProperty,TEUIProperty.UIState.CHECKED_AND_IN_USE);
            if (this.dependentProviderList != null) {
                for (TEPanelDataProvider dependentProvider : this.dependentProviderList) {
                    dependentProvider.unCheckAll();
                }
            }
        }
        return uiProperty.propertyList;
    }

    @Override
    public List<TEUIProperty.TESDKParam> getRevertData(Context context) {
        this.forceRefreshPanelData(context.getApplicationContext());
        return Collections.singletonList(ProviderUtils.createNoneItem(XmagicConstant.EffectName.EFFECT_MOTION));
    }

    @Override
    public List<TEUIProperty.TESDKParam> getCloseEffectItems(TEUIProperty uiProperty) {
        return Collections.singletonList(ProviderUtils.createNoneItem(XmagicConstant.EffectName.EFFECT_MOTION));
    }


    @Override
    public boolean isShowCompareBtn() {
        return false;
    }



    @Override
    public void putMutuallyExclusiveProvider(List<TEPanelDataProvider> providerList) {
        super.putMutuallyExclusiveProvider(providerList);
        this.dependentProviderList = providerList;
    }

    @Override
    public void unCheckAll() {
        super.unCheckAll();
        ProviderUtils.revertUIState(allData, null);
    }
}
