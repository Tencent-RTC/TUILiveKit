package com.trtc.uikit.livekit.component.gift.view;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;

import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.extension.TUILiveGiftManager;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.gift.view.adapter.GiftPanelAdapter;

import java.util.ArrayList;
import java.util.List;

/**
 * Gift panel viewpager management class
 */
public class GiftViewPagerManager {

    public GiftViewPagerManager() {

    }

    /**
     * Data for each page of the gift panel
     *
     * @param context    context
     * @param pageIndex  page index
     * @param gifts      gift list
     * @param columns    columns
     * @param rows       rows
     * @return RecycleView
     */
    public View viewPagerItem(final Context context, final int pageIndex, List<TUILiveGiftManager.GiftInfo> gifts,
                              int columns, int rows) {
        LayoutInflater inflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        @SuppressLint("InflateParams")
        View layout = inflater.inflate(R.layout.gift_layout_gift_panel, null);
        RecyclerView recyclerView = (RecyclerView) layout.findViewById(R.id.chart_face_gv);
        GridLayoutManager girdLayoutManager = new GridLayoutManager(context, columns);
        recyclerView.setLayoutManager(girdLayoutManager);
        int maxPageItems = columns * rows;
        int startIndex = pageIndex * maxPageItems;
        int endIndex = Math.min(maxPageItems * (pageIndex + 1), gifts.size());
        List<TUILiveGiftManager.GiftInfo> subList = new ArrayList<>(gifts.subList(startIndex, endIndex));
        final GiftPanelAdapter mGvAdapter = new GiftPanelAdapter(pageIndex, subList, context);
        recyclerView.setAdapter(mGvAdapter);
        mGvAdapter.setOnItemClickListener((view, gift, position, index) -> giftClickListener.onClick(position, gift));
        return recyclerView;
    }

    public interface GiftClickListener {
        void onClick(int position, TUILiveGiftManager.GiftInfo gift);
    }

    private GiftClickListener giftClickListener;

    public void setGiftClickListener(GiftClickListener listener) {
        giftClickListener = listener;
    }

    public int getPagerCount(int listSize, int columns, int rows) {
        return listSize % (columns * rows) == 0 ? listSize / (columns * rows) : listSize / (columns * rows) + 1;
    }
}
