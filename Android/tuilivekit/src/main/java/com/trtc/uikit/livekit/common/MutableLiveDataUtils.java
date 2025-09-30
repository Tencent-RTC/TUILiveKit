package com.trtc.uikit.livekit.common;

import androidx.lifecycle.MutableLiveData;

public class MutableLiveDataUtils {
    public static <T> void setValue(MutableLiveData<T> liveData, T value) {
        if (liveData == null) {
            return;
        }
        T currentValue = liveData.getValue();
        if (currentValue == null) {
            if (value != null) {
                liveData.setValue(value);
            }
        } else {
            if (!currentValue.equals(value)) {
                liveData.setValue(value);
            }
        }
    }
}
