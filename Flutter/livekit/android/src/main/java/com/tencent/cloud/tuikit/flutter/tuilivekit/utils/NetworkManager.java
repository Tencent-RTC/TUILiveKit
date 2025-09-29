package com.tencent.cloud.tuikit.flutter.tuilivekit.utils;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.net.ConnectivityManager;
import android.net.ConnectivityManager.NetworkCallback;
import android.net.Network;
import android.net.NetworkCapabilities;
import android.net.NetworkInfo;
import android.net.NetworkRequest;
import android.os.Build;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;

import java.util.logging.Logger;
import java.io.IOException;

import io.flutter.plugin.common.EventChannel;

public class NetworkManager implements EventChannel.StreamHandler {
    private static final String TAG = "NetworkManager";
    private static final String CONNECTED = "connected";
    private static final String DISCONNECTED = "disconnected";
    private static final int MAX_RETRIES = 3;
    private static final int DELAY_MS = 500;
    private final Context mContext;
    private ConnectivityManager mConnectivityManager;
    private NetworkCallback mNetworkCallback;
    private BroadcastReceiver mNetworkReceiver;
    private EventChannel.EventSink mEventSink;
    private Handler mMainHandler = new Handler(Looper.getMainLooper());

    public NetworkManager(Context context) {
        mContext = context;
        mConnectivityManager = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
    }

    public String getCurrentNetworkState() {
        if (mConnectivityManager == null) {
            Log.i(TAG, "getCurrentNetworkState. connectivityManager == null ");
            return DISCONNECTED;
        }

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
            Network network = mConnectivityManager.getActiveNetwork();
            if (network == null) {
                Log.i(TAG, "getCurrentNetworkState. network == null ");
                return DISCONNECTED;
            }
            NetworkCapabilities capabilities = mConnectivityManager.getNetworkCapabilities(network);
            if (capabilities == null) {
                Log.i(TAG, "getCurrentNetworkState. capabilities == null ");
                return DISCONNECTED;
            }
            boolean connected = capabilities.hasCapability(NetworkCapabilities.NET_CAPABILITY_INTERNET) && (
                    capabilities.hasTransport(NetworkCapabilities.TRANSPORT_WIFI) ||
                            capabilities.hasTransport(NetworkCapabilities.TRANSPORT_CELLULAR) ||
                            capabilities.hasTransport(NetworkCapabilities.TRANSPORT_ETHERNET)
            );
            Log.i(TAG, "getCurrentNetworkState. isConnected:" + connected);
            return connected ? CONNECTED : DISCONNECTED;
        } else {
            NetworkInfo networkInfo = mConnectivityManager.getActiveNetworkInfo();
            boolean connected = networkInfo != null && networkInfo.isConnected();
            Log.i(TAG, "getCurrentNetworkState VERSION<VERSION_CODES.N. isConnected:" + connected);
            return connected ? CONNECTED : DISCONNECTED;
        }
    }

    @Override
    public void onListen(Object arguments, EventChannel.EventSink events) {
        mEventSink = events;
        startListenNetworkConnection();
    }

    @Override
    public void onCancel(Object arguments) {
        mEventSink = null;
        stopListenNetworkConnection();
    }

    private void startListenNetworkConnection() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
            registerNetworkCallback();
        } else {
            registerNetworkBroadcastReceiver();
        }
    }

    private void stopListenNetworkConnection() {
        if (mConnectivityManager == null) {
            return;
        }
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
            if (mConnectivityManager != null && mNetworkCallback != null) {
                mConnectivityManager.unregisterNetworkCallback(mNetworkCallback);
                mNetworkCallback = null;
            }
        } else {
            if (mNetworkReceiver != null) {
                mContext.unregisterReceiver(mNetworkReceiver);
                mNetworkReceiver = null;
            }
        }
    }

    private void registerNetworkCallback() {
        if (mConnectivityManager == null) {
            return;
        }

        mNetworkCallback = new NetworkCallback() {
            private boolean lastConnected = false;

            @Override
            public void onAvailable(Network network) {
                Log.i(TAG, "registerNetworkCallback.mNetworkCallback.onAvailable");
                lastConnected = true;
                sendNetworkConnectionEvent(CONNECTED);
            }

            @Override
            public void onLost(Network network) {
                Log.i(TAG, "registerNetworkCallback.mNetworkCallback.onLost");
                boolean stillConnected = pingWithRetry(MAX_RETRIES, DELAY_MS);
                if (lastConnected != stillConnected) {
                    lastConnected = stillConnected;
                    sendNetworkConnectionEvent(stillConnected ? CONNECTED : DISCONNECTED);
                }
            }
        };
        NetworkRequest networkRequest = new NetworkRequest.Builder()
                .addCapability(NetworkCapabilities.NET_CAPABILITY_INTERNET)
                .build();
        mConnectivityManager.registerNetworkCallback(networkRequest, mNetworkCallback);
    }

    private void registerNetworkBroadcastReceiver() {
        mNetworkReceiver = new BroadcastReceiver() {
            private boolean lastConnected = false;

            @Override
            public void onReceive(Context context, Intent intent) {
                String networkState = getCurrentNetworkState();
                Log.i(TAG, "registerNetworkBroadcastReceiver.onReceive networkState:" + networkState);
                boolean connected = networkState == CONNECTED;
                if (lastConnected != connected) {
                    lastConnected = connected;
                    sendNetworkConnectionEvent(networkState);
                }
            }
        };
        IntentFilter intentFilter = new IntentFilter(ConnectivityManager.CONNECTIVITY_ACTION);
        mContext.registerReceiver(mNetworkReceiver, intentFilter);
    }

    private void sendNetworkConnectionEvent(String connectionStatus) {
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                if (mEventSink != null) {
                    Log.i(TAG, "sendNetworkConnectionEvent connectionStatus:" + connectionStatus);
                    mEventSink.success(connectionStatus);
                }
            }
        };
        mMainHandler.post(runnable);
    }

    private boolean pingWithRetry(int maxRetries, long delayMs) {
        for (int i = 0; i < maxRetries; i++) {
            try {
                Thread.sleep(delayMs);
            } catch (InterruptedException e) {
                Log.e(TAG, "pingWithRetry failed. current retry times:" + i);
            }

            if (pingToCheckRealConnectivity()) {
                return true;
            }
        }
        return false;
    }

    private boolean pingToCheckRealConnectivity() {
        try {
            Process process = Runtime.getRuntime().exec("/system/bin/ping -c 1 8.8.8.8");
            int result = process.waitFor();
            Log.i(TAG, "pingToCheckRealConnectivity result:" + result);
            return result == 0;
        } catch (IOException | InterruptedException e) {
            return false;
        }
    }
}
