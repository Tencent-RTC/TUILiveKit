package com.trtc.tuikit.common.livedata;

public interface Observer<T> {
    void onChanged(T t);
}