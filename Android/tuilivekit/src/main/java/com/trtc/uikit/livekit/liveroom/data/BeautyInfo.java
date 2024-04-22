package com.trtc.uikit.livekit.liveroom.data;

import androidx.annotation.NonNull;

import com.trtc.tuikit.common.livedata.LiveData;

public class BeautyInfo {
    public LiveData<Integer> smoothLevel    = new LiveData<>(6);
    public LiveData<Integer> whitenessLevel = new LiveData<>(6);
    public LiveData<Integer> ruddyLevel     = new LiveData<>(6);

    @NonNull
    @Override
    public String toString() {
        return ("BeautyInfo {"
                + "smoothLevel='" + smoothLevel.get()
                + ", whitenessLevel='" + whitenessLevel.get()
                + ", ruddyLevel='" + ruddyLevel.get()
                + '}');
    }
}
