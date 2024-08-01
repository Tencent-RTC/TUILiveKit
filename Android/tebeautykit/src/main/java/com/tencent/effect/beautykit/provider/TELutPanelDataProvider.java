package com.tencent.effect.beautykit.provider;

import android.content.Context;


import com.tencent.xmagic.XmagicConstant;
import com.tencent.effect.beautykit.model.TEUIProperty;
import com.tencent.effect.beautykit.utils.provider.ProviderUtils;

import java.util.Collections;
import java.util.List;


public class TELutPanelDataProvider extends TEAbstractPanelDataProvider {

    @Override
    public List<TEUIProperty> onItemClick(TEUIProperty uiProperty) {
        if (uiProperty == null) {
            return null;
        }
        if ((uiProperty.propertyList == null && uiProperty.sdkParam != null) || uiProperty.isNoneItem()) {
            ProviderUtils.revertUIState(allData, uiProperty);
            ProviderUtils.changeParamUIState(uiProperty, TEUIProperty.UIState.CHECKED_AND_IN_USE);
        }
        return uiProperty.propertyList;
    }

    @Override
    public List<TEUIProperty.TESDKParam> getRevertData(Context context) {
        this.forceRefreshPanelData(context);
        return Collections.singletonList(ProviderUtils.createNoneItem(XmagicConstant.EffectName.EFFECT_LUT));
    }

    @Override
    public List<TEUIProperty.TESDKParam> getCloseEffectItems(TEUIProperty uiProperty) {
        return Collections.singletonList(ProviderUtils.createNoneItem(XmagicConstant.EffectName.EFFECT_LUT));
    }


}
