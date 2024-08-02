package com.tencent.effect.beautykit.utils;

import android.os.Handler;
import android.os.HandlerThread;
import android.os.Message;

import androidx.annotation.NonNull;

public class WorkThread {

    private HandlerThread handlerThread = new HandlerThread("work_thread");
    private Handler handler = null;

    public static final int DEFAULT_WHAT = -100;

    private WorkThread() {
        handlerThread.start();
        handler = new Handler(handlerThread.getLooper()) {
            @Override
            public void handleMessage(@NonNull Message msg) {
                super.handleMessage(msg);
                if (msg.obj instanceof Runnable) {
                    ((Runnable) msg.obj).run();
                }
            }
        };
    }

    static class ClassHolder {
        static final WorkThread WORK_THREAD = new WorkThread();
    }

    public static WorkThread getInstance() {
        return ClassHolder.WORK_THREAD;
    }

    public void run(Runnable runnable, int what) {
        if (runnable == null) {
            return;
        }
        Message message = Message.obtain();
        message.obj = runnable;
        message.what = what;
        handler.sendMessage(message);
    }

    public void run(Runnable runnable) {
        if (runnable == null) {
            return;
        }
        Message message = Message.obtain();
        message.obj = runnable;
        message.what = DEFAULT_WHAT;
        handler.sendMessage(message);
    }

    public void cancel(Runnable runnable) {
        if (runnable == null) {
            return;
        }
        handler.removeCallbacks(runnable);
    }

    public void cancel(int what) {
        handler.removeMessages(what);
    }


}
