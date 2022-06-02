package com.tencent.qcloud.tuikit.tuiaudioeffect.model;

/**
 * 背景音乐数据Item
 */
public class BGMItemEntity {

    public String mTitle; // 歌曲名
    public String mPath;  // 路径

    public BGMItemEntity(String title, String path) {
        mTitle = title;
        mPath = path;
    }
}
