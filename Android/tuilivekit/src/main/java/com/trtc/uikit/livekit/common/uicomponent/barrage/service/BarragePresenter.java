package com.trtc.uikit.livekit.common.uicomponent.barrage.service;

import android.content.Context;

import com.trtc.uikit.livekit.common.uicomponent.barrage.model.TUIBarrage;
import com.trtc.uikit.livekit.common.uicomponent.barrage.view.IBarrageDisplayView;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;

public class BarragePresenter implements IBarragePresenter, IBarrageMessage.BarrageMessageDelegate {
    private static final String TAG = "BarragePresenter";

    protected final Context          mContext;
    private IBarrageDisplayView      mDisplayView;
    private final IBarrageMessage    mBarrageService;

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
    public void sendBarrage(final TUIBarrage barrage, final IBarrageMessage.BarrageSendCallBack callback) {
        mBarrageService.sendBarrage(barrage, new IBarrageMessage.BarrageSendCallBack() {

            @Override
            public void onSuccess(TUIBarrage barrage) {
                callback.onSuccess(barrage);
            }

            @Override
            public void onFailed(int code, String msg) {
                callback.onFailed(code, msg);
                LiveKitLog.debug(TAG + " sendBarrage failed errorCode = " + code + " , errorMsg = " + msg);
            }
        });
    }

    @Override
    public void onReceivedBarrage(TUIBarrage barrage) {
        receiveBarrage(barrage);
    }

    @Override
    public void receiveBarrage(TUIBarrage barrage) {
        if (barrage == null || barrage.content == null) {
            LiveKitLog.debug(TAG + " receiveBarrage barrage is empty");
            return;
        }
        if (mDisplayView != null) {
            mDisplayView.insertBarrages(barrage);
        }
    }
}
