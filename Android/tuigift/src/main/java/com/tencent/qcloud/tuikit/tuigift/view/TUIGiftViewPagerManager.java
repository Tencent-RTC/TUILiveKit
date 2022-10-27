package com.tencent.qcloud.tuikit.tuigift.view;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;

import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.qcloud.tuikit.tuigift.R;
import com.tencent.qcloud.tuikit.tuigift.model.TUIGiftModel;
import com.tencent.qcloud.tuikit.tuigift.view.adapter.TUIGiftPanelAdapter;

import java.util.ArrayList;
import java.util.List;

/**
 * 礼物面板viewpager管理类
 */
public class TUIGiftViewPagerManager {

    public TUIGiftViewPagerManager() {

    }

    /**
     * 礼物面板每一页的数据
     *
     * @param context       context
     * @param pageIndex     第几页
     * @param tuiGiftModels 礼物集合
     * @param columns       列数
     * @param rows          行数
     * @return 礼物播放RecycleView
     */
    public View viewPagerItem(final Context context, final int pageIndex, List<TUIGiftModel> tuiGiftModels,
                              int columns, int rows) {
        LayoutInflater inflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        View layout = inflater.inflate(R.layout.tuigift_layout_gift_panel, null);
        RecyclerView recyclerView = (RecyclerView) layout.findViewById(R.id.chart_face_gv);
        GridLayoutManager girdLayoutManager = new GridLayoutManager(context, columns);
        recyclerView.setLayoutManager(girdLayoutManager);
        List<TUIGiftModel> subList = new ArrayList<>();
        int maxPageItems = columns * rows;
        int startIndex = pageIndex * maxPageItems;
        int endIndex = Math.min(maxPageItems * (pageIndex + 1), tuiGiftModels.size());
        subList.addAll(tuiGiftModels.subList(startIndex, endIndex));
        final TUIGiftPanelAdapter mGvAdapter = new TUIGiftPanelAdapter(pageIndex, subList, context);
        recyclerView.setAdapter(mGvAdapter);
        mGvAdapter.setOnItemClickListener(new TUIGiftPanelAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(View view, TUIGiftModel giftModel, int position, int index) {
                giftClickListener.onClick(position, giftModel);
            }
        });
        return recyclerView;
    }

    public interface GiftClickListener {
        void onClick(int position, TUIGiftModel giftModel);
    }


    private GiftClickListener giftClickListener;

    public void setGiftClickListener(GiftClickListener listener) {
        giftClickListener = listener;
    }

    /**
     * 根据礼物数量以及GridView设置的行数和列数计算Pager数量
     *
     * @return pager数量
     */
    public int getPagerCount(int listSize, int columns, int rows) {
        return listSize % (columns * rows) == 0 ? listSize / (columns * rows) : listSize / (columns * rows) + 1;
    }
}
