package com.trtc.uikit.livekit.service;

import com.trtc.uikit.livekit.service.impl.RoomEngineService;

public class ServiceProvider {

    private ServiceProvider() {
    }

    private static class ServiceHolder {
        private static final ServiceProvider instance = new ServiceProvider();
    }

    public static ServiceProvider getInstance() {
        return ServiceHolder.instance;
    }

    public ILiveService getLiveService() {
        return new RoomEngineService();
    }
}
