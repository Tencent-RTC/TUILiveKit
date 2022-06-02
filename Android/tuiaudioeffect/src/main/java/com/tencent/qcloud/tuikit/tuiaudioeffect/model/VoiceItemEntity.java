package com.tencent.qcloud.tuikit.tuiaudioeffect.model;

/**
 * 变声/混响数据Item
 */
public class VoiceItemEntity {

    public String mTitle;        // 标题
    public int    mIconId;       // 默认状态图片资源ID
    public int    mSelectIconId; // 选中状态图片资源ID
    public int    mType;         // 类型

    public VoiceItemEntity(String title, int iconId, int selectIconId, int type) {
        mTitle = title;
        mIconId = iconId;
        mSelectIconId = selectIconId;
        mType = type;
    }
}
