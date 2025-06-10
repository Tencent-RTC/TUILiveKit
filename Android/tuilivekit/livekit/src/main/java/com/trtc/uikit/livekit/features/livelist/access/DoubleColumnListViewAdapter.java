package com.trtc.uikit.livekit.features.livelist.access;

import android.content.Context;
import android.view.View;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.trtc.uikit.livekit.features.livelist.LiveListViewDefine;

public class DoubleColumnListViewAdapter implements LiveListViewDefine.LiveListViewAdapter {
    private final Context mContext;

    public DoubleColumnListViewAdapter(Context context) {
        mContext = context;
    }

    @Override
    public View createLiveInfoView(TUILiveListManager.LiveInfo liveInfo) {
        DoubleColumnWidgetView widgetView = new DoubleColumnWidgetView(mContext);
        widgetView.init(liveInfo);
        return widgetView;
    }

    @Override
    public void updateLiveInfoView(View view, TUILiveListManager.LiveInfo liveInfo) {
        DoubleColumnWidgetView widgetView = (DoubleColumnWidgetView) view;
        widgetView.updateLiveInfoView(liveInfo);
    }
}
