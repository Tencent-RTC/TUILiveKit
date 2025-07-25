package com.trtc.uikit.livekit.features.livelist.view.liveListviewpager;

import android.annotation.SuppressLint;
import android.text.TextUtils;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentActivity;
import androidx.fragment.app.FragmentManager;
import androidx.viewpager2.adapter.FragmentStateAdapter;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.livelist.LiveListViewDefine;
import com.trtc.uikit.livekit.features.livelist.manager.LiveInfoListService;

import java.util.ArrayList;
import java.util.List;

public abstract class LiveListViewPagerAdapter extends FragmentStateAdapter {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getCommonLogger("LiveListViewPagerAdapter");

    private final List<LiveInfo> mLiveInfoList;

    private final FragmentManager     mFragmentManager;
    private final LiveInfoListService mLiveInfoListService;

    private boolean               mIsDataLoaded;
    private boolean               mIsLoading;
    private OnDataChangedListener mOnDataChangedListener;

    public LiveListViewPagerAdapter(@NonNull FragmentActivity fragmentActivity,
                                    LiveInfoListService liveInfoListService) {
        this(fragmentActivity, liveInfoListService, null);
    }

    public LiveListViewPagerAdapter(@NonNull FragmentActivity fragmentActivity,
                                    LiveInfoListService liveInfoListService, LiveInfo liveInfo) {
        super(fragmentActivity);
        mFragmentManager = fragmentActivity.getSupportFragmentManager();
        mLiveInfoListService = liveInfoListService;
        mLiveInfoList = new ArrayList<>();
        List<LiveInfo> liveInfoList = liveInfoListService.getLiveList();
        if (!liveInfoList.isEmpty()) {
            addData(liveInfoList);
            mIsDataLoaded = true;
            return;
        }

        if (liveInfo != null) {
            mLiveInfoListService.setFirstData(liveInfo);
            List<LiveInfo> firstLiveInfoList = new ArrayList<>();
            firstLiveInfoList.add(liveInfo);
            addData(firstLiveInfoList);
        }
    }

    public Fragment getFragment(int position) {
        return mFragmentManager.findFragmentByTag("f" + position);
    }

    public List<LiveInfo> getDataList() {
        return mLiveInfoList;
    }

    public void fetchData() {
        LOGGER.info("fetchLiveList enableSliding:");
        if (mIsLoading) {
            LOGGER.info("is start fetch data, waiting");
            return;
        }
        if (mIsDataLoaded && TextUtils.isEmpty(mLiveInfoListService.getLiveListDataCursor())) {
            LOGGER.info("there is no more data");
            return;
        }
        mIsLoading = true;
        mLiveInfoListService.fetchLiveList(new LiveListViewDefine.LiveListCallback() {
            @Override
            public void onSuccess(String cursor, List<LiveInfo> liveInfoList) {
                int startPosition = mLiveInfoList.size();
                mLiveInfoList.addAll(liveInfoList);
                notifyItemRangeInserted(startPosition, mLiveInfoList.size());
                mIsDataLoaded = true;
                mIsLoading = false;
            }

            @Override
            public void onError(int code, String message) {
                LOGGER.error("fetchLiveList failed,errorCode:" + code + ",message:" + message);
                mIsLoading = false;
            }
        });
    }

    public void refreshData(ActionCallback callback) {
        LOGGER.info("refreshData enableSliding:");
        mIsLoading = true;
        mLiveInfoListService.refreshLiveList(new LiveListViewDefine.LiveListCallback() {
            @SuppressLint("NotifyDataSetChanged")
            @Override
            public void onSuccess(String cursor, List<LiveInfo> liveInfoList) {
                mLiveInfoList.clear();
                mLiveInfoList.addAll(liveInfoList);
                updateLiveList(liveInfoList);
                if (callback != null) {
                    callback.onComplete();
                }
                mIsDataLoaded = true;
                mIsLoading = false;
            }

            @Override
            public void onError(int code, String message) {
                LOGGER.error("refreshData failed,errorCode:" + code + ",message:" + message);
                if (callback != null) {
                    callback.onComplete();
                }
                mIsLoading = false;
            }
        });
    }

    @NonNull
    @Override
    public Fragment createFragment(int position) {
        return new LiveListFragment(mLiveInfoList.get(position), this);
    }

    @SuppressLint("NotifyDataSetChanged")
    private void updateLiveList(List<LiveInfo> liveInfoList) {
        for (int i = 0; i < liveInfoList.size(); i++) {
            LiveListFragment fragment = (LiveListFragment) getFragment(i);
            if (fragment != null) {
                fragment.updateLiveInfo(liveInfoList.get(i));
            }
        }
        notifyDataSetChanged();
        if (mOnDataChangedListener != null) {
            mOnDataChangedListener.OnDataChanged();
        }
    }

    @Override
    public int getItemCount() {
        return mLiveInfoList.size();
    }

    public abstract View createLiveInfoView(LiveInfo liveInfo);

    public abstract void updateLiveInfoView(View view, LiveInfo liveInfo);

    public void onViewWillSlideIn(View view) {

    }

    public void onViewDidSlideIn(View view) {

    }

    public void onViewSlideInCancelled(View view) {
    }

    public void onViewWillSlideOut(View view) {

    }

    public void onViewDidSlideOut(View view) {

    }

    public void onViewSlideOutCancelled(View view) {

    }

    public void setOnDataChangedListener(OnDataChangedListener listener) {
        mOnDataChangedListener = listener;
    }

    private void addData(List<LiveInfo> liveInfoList) {
        int startPosition = mLiveInfoList.size();
        mLiveInfoList.addAll(liveInfoList);
        notifyItemRangeInserted(startPosition, mLiveInfoList.size());
    }

    public interface ActionCallback {
        void onComplete();
    }

    public interface OnDataChangedListener {
        void OnDataChanged();
    }
}
