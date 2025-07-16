package com.trtc.uikit.livekit.features.livelist.access;

import android.content.Context;
import android.view.View;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.trtc.uikit.livekit.features.livelist.LiveListViewDefine;

public class SingleColumnListViewAdapter implements LiveListViewDefine.LiveListViewAdapter {
    private final Context mContext;

    public SingleColumnListViewAdapter(Context context) {
        mContext = context;
    }

    @Override
    public View createLiveInfoView(TUILiveListManager.LiveInfo liveInfo) {
        SingleColumnWidgetView widgetView = new SingleColumnWidgetView(mContext);
        widgetView.init(liveInfo);
        return widgetView;
    }

    @Override
    public void updateLiveInfoView(View view, TUILiveListManager.LiveInfo liveInfo) {
        SingleColumnWidgetView widgetView = (SingleColumnWidgetView) view;
        widgetView.updateLiveInfoView(liveInfo);
    }
}
