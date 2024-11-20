package com.trtc.uikit.livekit.component.barrage.service;

import android.content.Context;
import android.util.Log;

import com.trtc.uikit.livekit.component.barrage.store.model.Barrage;
import com.trtc.uikit.livekit.component.barrage.view.IBarrageDisplayView;

public class BarragePresenter implements IBarragePresenter, IBarrageMessage.BarrageMessageDelegate {
    private static final String TAG = "BarragePresenter";

    protected final Context             mContext;
    private         IBarrageDisplayView mDisplayView;
    private final   IBarrageMessage     mBarrageService;

    public BarragePresenter(Context context, IBarrageMessage service) {
        mContext = context;
        mBarrageService = service;
    }

    @Override
    public void initDisplayView(IBarrageDisplayView view) {
        mDisplayView = view;
        mBarrageService.setDelegate(this);
    }

    @Override
    public void destroyPresenter() {
        mDisplayView = null;
        mBarrageService.setDelegate(null);
    }

    @Override
    public void sendBarrage(final Barrage barrage, final IBarrageMessage.BarrageSendCallBack callback) {
        mBarrageService.sendBarrage(barrage, new IBarrageMessage.BarrageSendCallBack() {

            @Override
            public void onSuccess(Barrage barrage) {
                callback.onSuccess(barrage);
            }

            @Override
            public void onFailed(int code, String msg) {
                callback.onFailed(code, msg);
                Log.i(TAG, "sendBarrage failed errorCode = " + code + " , errorMsg = " + msg);
            }
        });
    }

    @Override
    public void onReceivedBarrage(Barrage barrage) {
        receiveBarrage(barrage);
    }

    @Override
    public void receiveBarrage(Barrage barrage) {
        if (barrage == null || barrage.content == null) {
            Log.i(TAG, "receiveBarrage barrage is empty");
            return;
        }
        if (mDisplayView != null) {
            mDisplayView.insertBarrages(barrage);
        }
    }
}
