package com.tencent.effect.beautykit.utils.provider;

import android.text.TextUtils;

import com.tencent.effect.beautykit.TEBeautyKit;
import com.tencent.xmagic.XmagicConstant;
import com.tencent.effect.beautykit.model.TEMotionDLModel;
import com.tencent.effect.beautykit.model.TEUIProperty;
import com.tencent.effect.beautykit.utils.FileUtil;

import java.util.ArrayList;
import java.util.List;

public class ProviderUtils {

    private static final String TAG = ProviderUtils.class.getName();
    private static final String[] BEAUTY_WHITEN_EFFECT_NAMES = {
            XmagicConstant.EffectName.BEAUTY_WHITEN,
            XmagicConstant.EffectName.BEAUTY_WHITEN_2,
            XmagicConstant.EffectName.BEAUTY_WHITEN_3,

    };
    private static final String[] BEAUTY_FACE_EFFECT_NAMES = {
              XmagicConstant.EffectName.BEAUTY_FACE_NATURE,
              XmagicConstant.EffectName.BEAUTY_FACE_GODNESS,
              XmagicConstant.EffectName.BEAUTY_FACE_MALE_GOD,

    };


    public static final String HTTP_NAME = "http";
    public static final String ZIP_NAME = ".zip";


    private static boolean contains(String[] names, String effectName) {
        for (String name : names) {
            if (name.equals(effectName)) {
                return true;
            }
        }
        return false;
    }


    /**
     Find the data corresponding to the given category in allData.
     @param allData The complete set of data
     @param uiCategory The category to search for
     @return The data corresponding to the given category
     */
    public static TEUIProperty getUIPropertyByCategory(List<TEUIProperty> allData, TEUIProperty.UICategory uiCategory) {
        for (TEUIProperty uiProperty : allData) {
            if (uiCategory == uiProperty.uiCategory) {
                return uiProperty;
            }
        }
        return null;
    }



    public static void changeParamUIState(TEUIProperty teuiProperty, int uiState) {
        if (teuiProperty == null) {
            return;
        }
        teuiProperty.setUiState(uiState);
        ProviderUtils.changeParamUIState(teuiProperty.parentUIProperty, uiState);
    }



    public static void revertUIState(List<TEUIProperty> uiPropertyList, TEUIProperty currentItem) {
        if (uiPropertyList == null) {
            return;
        }
        for (TEUIProperty property : uiPropertyList) {
            if (property == null) {
                continue;
            }
            ProviderUtils.revertUIState(property.propertyList, currentItem);

            if (property.getUiState() == TEUIProperty.UIState.INIT) {
                continue;
            }
            if (property.uiCategory == TEUIProperty.UICategory.BEAUTY
                    || property.uiCategory == TEUIProperty.UICategory.BODY_BEAUTY) {
                if (isSameEffectName(property, currentItem)) {
                    changeParamUIState(property, TEUIProperty.UIState.INIT);
                } else {
                    changeParamUIState(property, TEUIProperty.UIState.IN_USE);
                }
            } else {
                changeParamUIState(property, TEUIProperty.UIState.INIT);
            }
        }
    }


    private static boolean isSameEffectName(TEUIProperty property, TEUIProperty property2) {
        if (property == null || property2 == null) {
            return false;
        }
        if (property.sdkParam == null || property2.sdkParam == null) {
            return false;
        }
        if (property.sdkParam.effectName.equals(property2.sdkParam.effectName)) {
            return true;
        }
        if (ProviderUtils.contains(BEAUTY_WHITEN_EFFECT_NAMES, property.sdkParam.effectName)
                && ProviderUtils.contains(BEAUTY_WHITEN_EFFECT_NAMES, property2.sdkParam.effectName)) {
            return true;
        }
        if (ProviderUtils.contains(BEAUTY_FACE_EFFECT_NAMES, property.sdkParam.effectName)
                && ProviderUtils.contains(BEAUTY_FACE_EFFECT_NAMES, property2.sdkParam.effectName)) {
            return true;
        }
        return false;
    }


    /**
     * Create a clone of teParam based on the usage and set the effectValue to 0.
     *      @param usedList The list of teParam currently in effect
     *      @return A new clone of teParam with effectValue set to 0
     **/
    public static List<TEUIProperty.TESDKParam> clone0ValuedParam(List<TEUIProperty.TESDKParam> usedList) {
        if (usedList == null) {
            return null;
        }
        List<TEUIProperty.TESDKParam> resultList = new ArrayList<>();
        for (TEUIProperty.TESDKParam param : usedList) {
            try {
                TEUIProperty.TESDKParam cloneParam = param.clone();
                cloneParam.effectValue = 0;
                resultList.add(cloneParam);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return resultList;
    }



    public static void changParamValuedTo0(List<TEUIProperty.TESDKParam> usedList) {
        if (usedList == null) {
            return;
        }
        for (TEUIProperty.TESDKParam param : usedList) {
            param.effectValue = 0;
        }
    }



    public static List<TEUIProperty.TESDKParam> getUsedProperties(List<TEUIProperty> uiProperties) {
        List<TEUIProperty.TESDKParam> usedProperties = new ArrayList<>();
        getUsedProperties(uiProperties, usedProperties);
        return usedProperties;
    }



    private static void getUsedProperties(List<TEUIProperty> uiProperties, List<TEUIProperty.TESDKParam> properties) {
        if (uiProperties != null && uiProperties.size() > 0) {
            for (TEUIProperty uiProperty : uiProperties) {
                if (uiProperty == null) {
                    continue;
                }
                if (uiProperty.getUiState() != TEUIProperty.UIState.INIT && uiProperty.sdkParam != null) {
                    properties.add(uiProperty.sdkParam);
                }
                if (uiProperty.propertyList != null) {
                    getUsedProperties(uiProperty.propertyList, properties);
                }
            }
        }
    }



    public static void completionResPath(TEUIProperty uiProperty) {
        if (uiProperty == null) {
            return;
        }
        if (uiProperty.uiCategory == TEUIProperty.UICategory.BEAUTY
                || uiProperty.uiCategory == TEUIProperty.UICategory.BODY_BEAUTY) {
            return;
        }
        if (uiProperty.sdkParam != null && !TextUtils.isEmpty(uiProperty.sdkParam.resourcePath)) {
            if (!uiProperty.sdkParam.resourcePath.contains(TEBeautyKit.getResPath())) {
                uiProperty.sdkParam.resourcePath = TEBeautyKit.getResPath() + uiProperty.sdkParam.resourcePath;
            }
        }
    }





    public static void createDlModelAndSDKParam(TEUIProperty teuiProperty, TEUIProperty.UICategory uiCategory) {
        switch (uiCategory) {
            case LUT:
            case MAKEUP:
            case MOTION:
            case SEGMENTATION:
                if (!TextUtils.isEmpty(teuiProperty.resourceUri)) {
                    String downloadPath = ProviderUtils.getDownloadPath(teuiProperty);
                    if (teuiProperty.resourceUri.startsWith(HTTP_NAME)) {
                        teuiProperty.dlModel = new TEMotionDLModel(downloadPath,
                                FileUtil.getFileNameByHttpUrl(teuiProperty.resourceUri), teuiProperty.resourceUri);
                    }
                    if (teuiProperty.sdkParam == null) {
                        teuiProperty.sdkParam = new TEUIProperty.TESDKParam();
                    }
                    if (teuiProperty.resourceUri.startsWith(HTTP_NAME)) {
                        String fileName = FileUtil.getFileNameByHttpUrl(teuiProperty.resourceUri);
                        if (!TextUtils.isEmpty(fileName) && fileName.endsWith(ZIP_NAME)) {
                            fileName = teuiProperty.dlModel.getFileNameNoZip();
                        }
                        teuiProperty.sdkParam.resourcePath = downloadPath + fileName;
                    } else {
                        teuiProperty.sdkParam.resourcePath = teuiProperty.resourceUri;
                    }
                }
                break;
            default:
                break;
        }
    }


    private static String getDownloadPath(TEUIProperty teuiProperty) {
        if (teuiProperty == null) {
            return null;
        }
        if (!TextUtils.isEmpty(teuiProperty.downloadPath)) {
            return teuiProperty.downloadPath;
        } else {
            return ProviderUtils.getDownloadPath(teuiProperty.parentUIProperty);
        }
    }



    public static TEUIProperty.TESDKParam createNoneItem(String effectName) {
        TEUIProperty.TESDKParam param = new TEUIProperty.TESDKParam();
        param.effectName = effectName;
        return param;
    }



    public static boolean findFirstInUseItemAndMakeChecked(List<TEUIProperty> allData) {
        if (allData == null) {
            return false;
        }
        for (TEUIProperty teuiProperty : allData) {
            if (teuiProperty.propertyList != null) {
                if (findFirstInUseItemAndMakeChecked(teuiProperty.propertyList)) {
                    return true;
                }
            } else if (teuiProperty.sdkParam != null && teuiProperty.getUiState() == TEUIProperty.UIState.IN_USE) {
                teuiProperty.setUiState(TEUIProperty.UIState.CHECKED_AND_IN_USE);
                ProviderUtils.changeParentUIState(teuiProperty, TEUIProperty.UIState.CHECKED_AND_IN_USE);
                return true;
            }
        }
        return false;
    }



    public static void changeParentUIState(TEUIProperty current, int uiState) {
        TEUIProperty parent = current.parentUIProperty;
        if (parent != null) {
            parent.setUiState(uiState);
            ProviderUtils.changeParentUIState(parent, uiState);
        }
    }

}
