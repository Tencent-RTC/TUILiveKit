package com.tencent.effect.beautykit.tuiextension.utils;

import com.tencent.effect.beautykit.utils.LogUtils;

import java.util.Timer;
import java.util.TimerTask;


public class TimerManager {

    private final String TAG = TimerManager.class.getName();
    private int intervalTime = 30;
    private volatile TASK_STATE task_state = TASK_STATE.INIT;
    private Timer timer = null;
    private TimerManagerCallback mTimerCallback = null;

    public TimerManager(TimerManagerCallback managerCallback) {
        this.mTimerCallback = managerCallback;
    }

    public TimerManager(TimerManagerCallback managerCallback, int intervalTime) {
        this.mTimerCallback = managerCallback;
        this.intervalTime = intervalTime;
    }


    private TimerTask createTask() {
        return new TimerTask() {
            @Override
            public void run() {
                if (mTimerCallback != null) {
                    mTimerCallback.onCall();
                }
            }
        };
    }

    public void start() {
        LogUtils.d(TAG, "start");
        if (this.task_state == TASK_STATE.RELEASE || this.task_state == TASK_STATE.STARTED) {
            return;
        }
        this.task_state = TASK_STATE.STARTED;
        timer = new Timer();
        timer.scheduleAtFixedRate(this.createTask(), 0, intervalTime);
    }

    public void pause() {
        LogUtils.d(TAG, "pause");
        this.task_state = TASK_STATE.PAUSED;
        if (timer != null) {
            timer.cancel();
            timer.purge();
            timer = null;
        }
    }


    public void release() {
        LogUtils.d(TAG, "release");
        this.task_state = TASK_STATE.RELEASE;
        if (timer != null) {
            timer.cancel();
            timer.purge();
            timer = null;
        }
    }


    public interface TimerManagerCallback {
        void onCall();
    }


    private enum TASK_STATE {
        INIT, STARTED, PAUSED, RELEASE
    }

}
