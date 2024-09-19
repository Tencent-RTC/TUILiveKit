package com.trtc.uikit.livekit.example.view.main.model;

public class MainItemData {
    private final MainTypeEnum type;
    private final int          resId;
    private final int          title;
    private final int          subTitle;

    public MainItemData(MainTypeEnum itemType, int itemResId, int itemTitle, int itemSubTitle) {
        this.type = itemType;
        this.resId = itemResId;
        this.title = itemTitle;
        this.subTitle = itemSubTitle;
    }

    public MainTypeEnum getType() {
        return type;
    }

    public int getResId() {
        return resId;
    }

    public int getTitle() {
        return title;
    }

    public int getSubTitle() {
        return subTitle;
    }
}
