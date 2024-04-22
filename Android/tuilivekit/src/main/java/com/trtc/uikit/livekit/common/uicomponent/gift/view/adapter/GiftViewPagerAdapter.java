package com.trtc.uikit.livekit.common.uicomponent.gift.view.adapter;

import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.viewpager.widget.PagerAdapter;

import java.util.List;

public class GiftViewPagerAdapter extends PagerAdapter {

    private final List<View> mViewList;

    public GiftViewPagerAdapter(List<View> views) {
        this.mViewList = views;
    }

    @Override
    public int getCount() {
        return mViewList != null ? mViewList.size() : 0;
    }

    @Override
    public boolean isViewFromObject(@NonNull View view, @NonNull Object object) {
        return view == object;
    }

    @NonNull
    @Override
    public Object instantiateItem(ViewGroup container, int position) {
        container.addView(mViewList.get(position), 0);
        return mViewList.get(position);
    }

    @Override
    public void destroyItem(ViewGroup container, int position, @NonNull Object object) {
        container.removeView(mViewList.get(position));
    }
}