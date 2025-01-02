package com.trtc.uikit.livekit.component.floatwindow.core.impl;

import com.tencent.qcloud.tuicore.util.ScreenUtil;

public class FloatWindowConfig {
    public int windowWidth;
    public int windowHeight;
    public int marginToEdge;
    public int maxMoveDistanceOnClick;

    public boolean attachToSideWithAnimation;

    public FloatWindowConfig() {
        windowWidth = ScreenUtil.dip2px(100);
        windowHeight = ScreenUtil.dip2px(200);
        marginToEdge = ScreenUtil.dip2px(10);
        maxMoveDistanceOnClick = 10;
        attachToSideWithAnimation = false;
    }
}
