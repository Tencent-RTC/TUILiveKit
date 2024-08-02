package com.tencent.effect.beautykit.model;



import java.util.ArrayList;
import java.util.List;


public class TEPanelMenuModel {
    private List<TEPanelDataModel> motions;
    private List<TEPanelDataModel> beauties;
    private List<TEPanelDataModel> makeups;
    private List<TEPanelDataModel> lut;


    public List<TEPanelMenuCategory> getPanelMenuCategories() {
        List<TEPanelMenuCategory> list = new ArrayList<>();
        if (motions != null && motions.size() > 0) {
            list.add(TEPanelMenuCategory.MOTION);
        }
        if (beauties != null && beauties.size() > 0) {
            list.add(TEPanelMenuCategory.BEAUTY);
        }
        if (makeups != null && makeups.size() > 0) {
            list.add(TEPanelMenuCategory.MAKEUP);
        }
        if (lut != null && lut.size() > 0) {
            list.add(TEPanelMenuCategory.LUT);
        }
        return list;
    }


    public TEPanelMenuModel() {
    }

    public TEPanelMenuModel(List<TEPanelDataModel> motions, List<TEPanelDataModel> beauties,
                            List<TEPanelDataModel> makeups, List<TEPanelDataModel> lut) {
        this.motions = motions;
        this.beauties = beauties;
        this.makeups = makeups;
        this.lut = lut;
    }

    public List<TEPanelDataModel> getDataByType(TEPanelMenuCategory tePanelMenuCategory) {
        switch (tePanelMenuCategory) {
            case MOTION:
                return this.motions;
            case BEAUTY:
                return this.beauties;
            case MAKEUP:
                return this.makeups;
            case LUT:
                return this.lut;
            default:
                break;
        }
        return null;
    }


    public List<TEPanelDataModel> getAllData() {
        List<TEPanelDataModel> result = new ArrayList<>();
        if (this.motions != null) {
            result.addAll(this.motions);
        }
        if (this.beauties != null) {
            result.addAll(this.beauties);
        }
        if (this.makeups != null) {
            result.addAll(this.makeups);
        }
        if (this.lut != null) {
            result.addAll(this.lut);
        }
        return result;
    }
}
