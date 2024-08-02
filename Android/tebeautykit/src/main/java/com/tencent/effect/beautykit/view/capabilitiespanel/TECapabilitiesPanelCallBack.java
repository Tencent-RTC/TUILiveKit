package com.tencent.effect.beautykit.view.capabilitiespanel;



import com.tencent.effect.beautykit.model.TECapabilitiesModel;


public interface TECapabilitiesPanelCallBack {


    void onCheckedChanged(TECapabilitiesModel capabilitiesItem, boolean isChecked);



    void onCameraClick();
}
