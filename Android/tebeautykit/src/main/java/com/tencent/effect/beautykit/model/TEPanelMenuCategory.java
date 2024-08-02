package com.tencent.effect.beautykit.model;

import com.tencent.effect.beautykit.provider.TEBeautyPanelDataProvider;
import com.tencent.effect.beautykit.provider.TELutPanelDataProvider;
import com.tencent.effect.beautykit.provider.TEMakeUpPanelDataProvider;
import com.tencent.effect.beautykit.provider.TEMotionPanelDataProvider;

public enum TEPanelMenuCategory {


    MOTION(TEMotionPanelDataProvider.class.getName()),
    BEAUTY(TEBeautyPanelDataProvider.class.getName()),
    MAKEUP(TEMakeUpPanelDataProvider.class.getName()),
    CAMERA(null),
    LUT(TELutPanelDataProvider.class.getName());
    public String className;

    TEPanelMenuCategory(String providerClassName) {
        this.className = providerClassName;
    }

}
