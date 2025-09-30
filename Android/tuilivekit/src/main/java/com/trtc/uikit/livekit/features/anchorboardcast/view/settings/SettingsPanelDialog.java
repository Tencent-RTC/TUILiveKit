package com.trtc.uikit.livekit.features.anchorboardcast.view.settings;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Rect;
import android.view.LayoutInflater;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;
import io.trtc.tuikit.atomicxcore.api.LiveCoreView;

@SuppressLint("ViewConstructor")
public class SettingsPanelDialog extends PopupDialog {

    private final Context       mContext;
    private final AnchorManager mLiveManager;
    private final LiveCoreView  mLiveCoreView;

    public SettingsPanelDialog(@NonNull Context context, AnchorManager manager, LiveCoreView liveCoreView) {
        super(context);
        mContext = context;
        mLiveManager = manager;
        mLiveCoreView = liveCoreView;
        initView();
    }

    protected void initView() {
        View view = LayoutInflater.from(getContext()).inflate(R.layout.livekit_anchor_settings_panel, null);

        initSettingsListView(view);
        setView(view);
    }

    private void initSettingsListView(View view) {
        RecyclerView mRecycleSettingsList = view.findViewById(R.id.rv_settings_list);
        final int spanCount = 5;
        mRecycleSettingsList.setLayoutManager(new GridLayoutManager(getContext(), spanCount));
        int screenWidth = ScreenUtil.getScreenWidth(getContext());
        int itemWidth = ScreenUtil.dip2px(56);
        int spanSpace0 = (screenWidth - spanCount * itemWidth) / (spanCount);
        int spanSpace1 = (screenWidth - spanCount * itemWidth) / (spanCount + 1);
        mRecycleSettingsList.addItemDecoration(new RecyclerView.ItemDecoration() {
            @Override
            public void getItemOffsets(@NonNull Rect outRect, @NonNull View view,
                                       @NonNull RecyclerView parent, @NonNull RecyclerView.State state) {
                int position = parent.getChildLayoutPosition(view) % spanCount;
                outRect.left = (1 + position) * spanSpace1 - position * spanSpace0;
            }
        });
        SettingsListAdapter adapter = new SettingsListAdapter(mContext, mLiveManager, mLiveCoreView, this);
        mRecycleSettingsList.setAdapter(adapter);
    }
}
