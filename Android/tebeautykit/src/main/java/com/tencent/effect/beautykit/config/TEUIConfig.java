package com.tencent.effect.beautykit.config;


import android.text.TextUtils;

import androidx.annotation.ColorInt;

import com.tencent.effect.beautykit.model.TEPanelDataModel;
import com.tencent.effect.beautykit.model.TEUIProperty;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

public class TEUIConfig {

    @ColorInt
    public int panelBackgroundColor = 0x66000000;  //Default background color
    @ColorInt
    public int panelDividerColor = 0x19FFFFFF;  //Divider color
    @ColorInt
    public int panelItemCheckedColor = 0xFF006EFF;  //Selected item color
    @ColorInt
    public int textColor = 0x99FFFFFF; //Text color
    @ColorInt
    public int textCheckedColor = 0xFFFFFFFF; //Text selected color
    @ColorInt
    public int seekBarProgressColor = 0xFF006EFF; //Progress bar color


    private Locale mLocale = Locale.getDefault();


    private List<TEPanelDataModel> defaultPanelDataList = new ArrayList<>();


    private static class ClassHolder {
        static final TEUIConfig TUI_CONFIG = new TEUIConfig();
    }

    public static TEUIConfig getInstance() {
        return ClassHolder.TUI_CONFIG;
    }


    public void setSystemLocal(Locale locale) {
        this.mLocale = locale;
    }


    /**
     Set the JSON file paths for the beauty panel.
     @param beauty The JSON file path for beauty attributes, set to null if not available.
     @param beautyBody The JSON file path for beauty body attributes, set to null if not available.
     @param lut The JSON file path for filter attributes, set to null if not available.
     @param motion The JSON file path for motion sticker attributes, set to null if not available.
     @param makeup The JSON file path for makeup attributes, set to null if not available.
     @param segmentation The JSON file path for segmentation attributes, set to null if not available.
     */
    public void setTEPanelViewRes(String beauty, String beautyBody, String lut, String motion, String makeup,
                                  String segmentation) {
        this.defaultPanelDataList.clear();
        if (!TextUtils.isEmpty(beauty)) {
            this.defaultPanelDataList.add(new TEPanelDataModel(beauty, TEUIProperty.UICategory.BEAUTY));
        }
        if (!TextUtils.isEmpty(beautyBody)) {
            this.defaultPanelDataList.add(new TEPanelDataModel(beautyBody, TEUIProperty.UICategory.BODY_BEAUTY));
        }
        if (!TextUtils.isEmpty(lut)) {
            this.defaultPanelDataList.add(new TEPanelDataModel(lut, TEUIProperty.UICategory.LUT));
        }
        if (!TextUtils.isEmpty(motion)) {
            this.defaultPanelDataList.add(new TEPanelDataModel(motion, TEUIProperty.UICategory.MOTION));
        }
        if (!TextUtils.isEmpty(makeup)) {
            this.defaultPanelDataList.add(new TEPanelDataModel(makeup, TEUIProperty.UICategory.MAKEUP));
        }
        if (!TextUtils.isEmpty(segmentation)) {
            this.defaultPanelDataList.add(new TEPanelDataModel(segmentation, TEUIProperty.UICategory.SEGMENTATION));
        }
    }

    public List<TEPanelDataModel> getPanelDataList() {
        return this.defaultPanelDataList;
    }


    public boolean isDefaultLanguage() {
        return this.isZh();
    }

    public boolean isZh() {
        return "zh".equals(mLocale.getLanguage());
    }

    public boolean isJA() {
        return "ja".equals(mLocale.getLanguage());
    }


}
