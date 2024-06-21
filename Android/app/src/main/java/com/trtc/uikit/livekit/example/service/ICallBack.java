package com.trtc.uikit.livekit.example.service;

public interface ICallBack {
    void onSuccess();

    void onError(int code, String desc);
}
