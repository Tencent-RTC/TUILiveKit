package com.tencent.qcloud.tuikit.tuigift.view.adapter;

import android.view.View;
import android.view.ViewGroup;

import androidx.viewpager.widget.PagerAdapter;

import java.util.List;

/**
 * 礼物面板viewpager adapter
 */
public class TUIGiftViewPagerAdapter extends PagerAdapter {

    private List<View> mViewList;

    public TUIGiftViewPagerAdapter(List<View> views) {
        this.mViewList = views;
    }

    @Override
    public int getCount() {
        return mViewList != null ? mViewList.size() : 0;
    }

    @Override
    public boolean isViewFromObject(View view, Object object) {
        return view == object;
    }

    @Override
    public Object instantiateItem(ViewGroup container, int position) {
        container.addView(mViewList.get(position), 0);
        return mViewList.get(position);
    }

    @Override
    public void destroyItem(ViewGroup container, int position, Object object) {
        container.removeView(mViewList.get(position));
    }
}