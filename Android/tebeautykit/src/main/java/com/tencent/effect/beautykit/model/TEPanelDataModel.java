package com.tencent.effect.beautykit.model;



public class TEPanelDataModel {

    public String jsonFilePath;
    public TEUIProperty.UICategory category;
    public String abilityType;

    public TEPanelDataModel() {
    }

    public TEPanelDataModel(String jsonFilePath, TEUIProperty.UICategory category) {
        this.jsonFilePath = jsonFilePath;
        this.category = category;
    }

    public TEPanelDataModel(String jsonFilePath, TEUIProperty.UICategory category, String abilityType) {
        this.jsonFilePath = jsonFilePath;
        this.category = category;
        this.abilityType = abilityType;
    }



}
