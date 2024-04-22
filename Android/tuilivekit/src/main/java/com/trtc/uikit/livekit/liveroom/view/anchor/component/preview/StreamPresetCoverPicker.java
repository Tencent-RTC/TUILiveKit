package com.trtc.uikit.livekit.liveroom.view.anchor.component.preview;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.DisplayMetrics;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.View;

import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.utils.Constants;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.view.anchor.component.preview.adapter.PresetImageGridAdapter;

import java.util.Arrays;
import java.util.List;

public class StreamPresetCoverPicker extends PopupDialog {

    private final LiveRoomInfo mLiveRoomInfo;
    private       String       mSelectedCoverURL;

    public StreamPresetCoverPicker(Context context, LiveRoomInfo roomInfo) {
        super(context);
        mLiveRoomInfo = roomInfo;

        initView(context);
    }

    private void initView(Context context) {
        @SuppressLint("InflateParams")
        View view = LayoutInflater.from(getContext()).inflate(R.layout.livekit_layout_select_preset_cover, null);

        initRecycleView(context, view);
        initBackButton(view);
        initSetCoverButton(view);
        setView(view);
    }

    private void initRecycleView(Context context, View view) {
        RecyclerView recyclerView = view.findViewById(R.id.rv_cover);
        recyclerView.setBackgroundResource(R.drawable.livekit_dialog_background);
        setView(recyclerView);

        int spanCount = calculateViewColumnCount(context);
        recyclerView.setLayoutManager(new GridLayoutManager(context, spanCount));
        recyclerView.addItemDecoration(new PresetImageGridAdapter.GridDividerItemDecoration(context));

        List<String> dataList = Arrays.asList(Constants.COVER_URL_LIST);
        recyclerView.setAdapter(new PresetImageGridAdapter(context, mLiveRoomInfo, dataList,
                coverURL -> mSelectedCoverURL = coverURL));
    }

    private void initBackButton(View view) {
        view.findViewById(R.id.iv_back).setOnClickListener(v -> dismiss());
    }

    private void initSetCoverButton(View view) {
        view.findViewById(R.id.btn_set_cover).setOnClickListener(v -> {
            mLiveRoomInfo.coverURL.set(mSelectedCoverURL);
            dismiss();
        });
    }

    private int calculateViewColumnCount(Context context) {
        DisplayMetrics metrics = context.getResources().getDisplayMetrics();
        int imageViewDP = 114;
        int screenWidth = metrics.widthPixels;
        int itemWidth = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, imageViewDP, metrics);
        return screenWidth / itemWidth;
    }

}
