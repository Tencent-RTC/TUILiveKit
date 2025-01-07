package com.trtc.uikit.livekit.voiceroomcore.manager.observer;

import com.trtc.uikit.livekit.voiceroomcore.SeatGridViewObserver;

import java.util.ArrayList;
import java.util.List;

public class SeatGridViewObserverManager {
    private final List<SeatGridViewObserver> observers = new ArrayList<>();

    public void addObserver(SeatGridViewObserver observer) {
        observers.add(observer);
    }

    public void removeObserver(SeatGridViewObserver observer) {
        observers.remove(observer);
    }

    public List<SeatGridViewObserver> getObservers() {
        return observers;
    }
}
