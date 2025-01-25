package com.trtc.uikit.livekit.component.liveListviewpager;

import android.content.Context;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.lifecycle.Lifecycle;
import androidx.viewpager2.adapter.FragmentStateAdapter;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;

import java.util.ArrayList;
import java.util.List;

public abstract class LiveListViewAdapter extends FragmentStateAdapter {
    private final LiveListDataSource mLiveListDataSource;
    private final List<LiveInfo>     mLiveInfoList;

    private final FragmentManager mFragmentManager;

    public LiveListViewAdapter(@NonNull Context context, @NonNull FragmentManager fragmentManager,
                               @NonNull Lifecycle lifecycle, LiveListDataSource dataSource) {
        super(fragmentManager, lifecycle);
        mLiveInfoList = new ArrayList<>();
        mFragmentManager = fragmentManager;
        mLiveListDataSource = dataSource;
        fetchData();
    }

    public Fragment getFragment(int position) {
        return mFragmentManager.findFragmentByTag("f" + position);
    }

    public List<LiveInfo> getDataList() {
        return mLiveInfoList;
    }

    public void fetchData() {
        if (mLiveListDataSource != null) {
            mLiveListDataSource.fetchLiveList(liveInfoList -> {
                if (liveInfoList == null) {
                    return;
                }
                if (liveInfoList.isEmpty()) {
                    return;
                }
                int startPosition = mLiveInfoList.size();
                mLiveInfoList.addAll(liveInfoList);
                notifyItemRangeInserted(startPosition, mLiveInfoList.size());
            });
        }
    }

    @NonNull
    @Override
    public Fragment createFragment(int position) {
        return new LiveListFragment(mLiveInfoList.get(position), this);
    }

    @Override
    public int getItemCount() {
        return mLiveInfoList.size();
    }

    public interface LiveListCallback {
        void onCompleted(List<LiveInfo> liveInfoList);
    }

    public interface LiveListDataSource {
        void fetchLiveList(LiveListCallback callback);
    }

    public abstract View onCreateView(LiveInfo liveInfo);

    public abstract void onViewWillSlideIn(View view);

    public abstract void onViewDidSlideIn(View view);

    public abstract void onViewSlideInCancelled(View view);

    public abstract void onViewWillSlideOut(View view);

    public abstract void onViewDidSlideOut(View view);

    public abstract void onViewSlideOutCancelled(View view);
}
