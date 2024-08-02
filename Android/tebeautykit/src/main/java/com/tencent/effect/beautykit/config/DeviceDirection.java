package com.tencent.effect.beautykit.config;


public enum DeviceDirection {

    PORTRAIT_UP(0),
    LANDSCAPE_RIGHT(90),
    PORTRAIT_DOWN(180),
    LANDSCAPE_LEFT(270);


    public final int angle;

    DeviceDirection(int angle) {
        this.angle = angle;
    }


}
