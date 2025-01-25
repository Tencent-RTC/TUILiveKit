package com.trtc.uikit.livekit.component.liveListviewpager;

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
    private static final String              TAG = "LiveListFragment";
    private final        LiveInfo            mLiveInfo;
    private final        LiveListViewAdapter mLiveListViewAdapter;

    private boolean mIsSlideIn;

    public LiveListFragment(LiveInfo liveInfo, LiveListViewAdapter adapter) {
        mLiveInfo = liveInfo;
        mLiveListViewAdapter = adapter;
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        if (mLiveListViewAdapter == null) {
            return new RelativeLayout(getContext());
        }
        return mLiveListViewAdapter.onCreateView(mLiveInfo);
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        if (mIsSlideIn) {
            if (mLiveListViewAdapter != null) {
                mLiveListViewAdapter.onViewDidSlideOut(getView());
            }
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
        mIsSlideIn = true;
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
        mIsSlideIn = false;
    }

    public void onFragmentSlideOutCancelled() {
        if (mLiveListViewAdapter != null) {
            mLiveListViewAdapter.onViewSlideOutCancelled(getView());
        }
    }
}
