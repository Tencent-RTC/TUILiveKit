package com.trtc.uikit.livekit.features.livelist.view.liveListviewpager;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;

public class LiveListFragment extends Fragment {
    private       LiveInfo                 mLiveInfo;
    private final LiveListViewPagerAdapter mLiveListViewAdapter;

    public LiveListFragment(LiveInfo liveInfo, LiveListViewPagerAdapter adapter) {
        mLiveInfo = liveInfo;
        mLiveListViewAdapter = adapter;
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        if (mLiveListViewAdapter == null) {
            return new RelativeLayout(getContext());
        }
        return mLiveListViewAdapter.createLiveInfoView(mLiveInfo);
    }

    public void updateLiveInfo(LiveInfo liveInfo) {
        mLiveInfo = liveInfo;
        if (mLiveListViewAdapter != null) {
            mLiveListViewAdapter.updateLiveInfoView(getView(), liveInfo);
        }
    }

    public void onFragmentWillSlideIn() {
        if (mLiveListViewAdapter != null) {
            mLiveListViewAdapter.onViewWillSlideIn(getView());
        }
    }

    public void onFragmentDidSlideIn() {
        if (mLiveListViewAdapter != null) {
            mLiveListViewAdapter.onViewDidSlideIn(getView());
        }
    }

    public void onFragmentSlideInCancelled() {
        if (mLiveListViewAdapter != null) {
            mLiveListViewAdapter.onViewSlideInCancelled(getView());
        }
    }

    public void onFragmentWillSlideOut() {
        if (mLiveListViewAdapter != null) {
            mLiveListViewAdapter.onViewWillSlideOut(getView());
        }
    }

    public void onFragmentDidSlideOut() {
        if (mLiveListViewAdapter != null) {
            mLiveListViewAdapter.onViewDidSlideOut(getView());
        }
    }

    public void onFragmentSlideOutCancelled() {
        if (mLiveListViewAdapter != null) {
            mLiveListViewAdapter.onViewSlideOutCancelled(getView());
        }
    }
}
