package com.trtc.uikit.livekit.livestream.view.anchor.pushing.settings;

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
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;

@SuppressLint("ViewConstructor")
public class SettingsPanelDialog extends PopupDialog {

    private final LiveStreamManager mLiveManager;

    public SettingsPanelDialog(@NonNull Context context, LiveStreamManager manager) {
        super(context);
        mLiveManager = manager;
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
                int position = parent.getChildLayoutPosition(view);
                outRect.left = (1 + position) * spanSpace1 - position * spanSpace0;
            }
        });
        SettingsListAdapter mAdapter = new SettingsListAdapter(getContext(), mLiveManager, this);
        mRecycleSettingsList.setAdapter(mAdapter);
    }
}
