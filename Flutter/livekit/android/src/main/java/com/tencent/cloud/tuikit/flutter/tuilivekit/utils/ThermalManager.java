package com.tencent.cloud.tuikit.flutter.tuilivekit.utils;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;

import io.flutter.plugin.common.EventChannel;

public class ThermalManager implements EventChannel.StreamHandler {
    private final Context context;
    private EventChannel.EventSink events;
    private BroadcastReceiver receiver;

    public ThermalManager(Context context) {
        this.context = context;
    }

    @Override
    public void onListen(Object arguments, EventChannel.EventSink events) {
        this.events = events;
        IntentFilter filter = new IntentFilter(Intent.ACTION_BATTERY_CHANGED);
        receiver = new BroadcastReceiver() {
            @Override
            public void onReceive(Context ctx, Intent intent) {
                int temp = intent.getIntExtra("temperature", -1);
                if (temp != -1 && events != null) {
                    double celsius = temp / 10.0;
                    events.success(celsius);
                }
            }
        };
        context.registerReceiver(receiver, filter);
    }

    @Override
    public void onCancel(Object arguments) {
        if (receiver != null) {
            context.unregisterReceiver(receiver);
            receiver = null;
        }
        events = null;
    }
}