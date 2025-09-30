package com.trtc.uikit.livekit.common;

import android.util.Log;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.arch.core.util.Function;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MediatorLiveData;

public class ReadOnlyLiveData<T> extends MediatorLiveData<T> {

    private static final String TAG = "ReadOnlyLiveData";

    @FunctionalInterface
    public interface DataRead<T> {
        T read();
    }

    private final DataRead<T>    dataReader;
    private final LiveData<T>    dataSource;
    private final Function<T, T> dataDuplicator;

    public ReadOnlyLiveData(@NonNull DataRead<T> reader, Function<T, T> duplicator) {
        super();
        dataReader = reader;
        dataSource = null;
        dataDuplicator = duplicator;
    }

    public ReadOnlyLiveData(@NonNull LiveData<T> data, Function<T, T> duplicator) {
        super();
        dataReader = null;
        dataSource = data;
        dataDuplicator = duplicator;
        addSource(dataSource, value -> {
            T newValue = dataDuplicator == null ? value : dataDuplicator.apply(value);
            super.setValue(newValue);
        });
    }

    public void destroy() {
        if (dataSource != null) {
            removeSource(dataSource);
        }
    }

    @Nullable
    @Override
    public T getValue() {
        T newValue = dataReader == null ? dataSource.getValue() : dataReader.read();
        return dataDuplicator == null ? super.getValue() : dataDuplicator.apply(newValue);
    }

    @Override
    public void setValue(T value) {
        Log.w(TAG, "setValue() should not be called externally");
    }

    @Override
    public void postValue(T value) {
        Log.w(TAG, "postValue() should not be called externally");
    }
}
