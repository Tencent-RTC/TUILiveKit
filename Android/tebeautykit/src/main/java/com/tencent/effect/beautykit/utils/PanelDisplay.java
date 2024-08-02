package com.tencent.effect.beautykit.utils;



import com.tencent.effect.beautykit.config.TEUIConfig;
import com.tencent.effect.beautykit.model.TECapabilitiesModel;
import com.tencent.effect.beautykit.model.TEUIProperty;



public class PanelDisplay {
    public static String getDisplayName(TEUIProperty uiProperty) {
        if (uiProperty == null) {
            return null;
        }
        String displayName = TEUIConfig.getInstance().isDefaultLanguage()
                ? uiProperty.displayName : uiProperty.displayNameEn;
        if (displayName == null) {
            displayName = uiProperty.displayName;
        }
        return displayName;
    }



    public static String getLabel(TECapabilitiesModel capabilitiesItem) {
        if (capabilitiesItem == null) {
            return null;
        }
        String label = TEUIConfig.getInstance().isDefaultLanguage() ? capabilitiesItem.label : capabilitiesItem.labelEn;
        if (label == null) {
            label = capabilitiesItem.label;
        }
        return label;
    }
}
