package com.tencent.effect.beautykit.tuiextension.utils;


public class AppConfig {
    public static final int TE_CHOOSE_PHOTO_SEG_CUSTOM = 2002;
    public static String PICK_CONTENT_ALL = "image/*|video/*";

    private AppConfig() {
    }

    private static class ClassHolder {
        static final AppConfig APP_CONFIG = new AppConfig();
    }

    public static AppConfig getInstance() {
        return ClassHolder.APP_CONFIG;
    }
}
