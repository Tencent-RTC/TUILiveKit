package com.trtc.uikit.livekit.features.audiencecontainer.view.liveListviewpager;

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
import com.trtc.uikit.livekit.features.audiencecontainer.AudienceContainerViewDefine;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.module.LiveInfoListService;
import com.trtc.uikit.livekit.features.audiencecontainer.state.AudienceContainerConfig;

import java.util.ArrayList;
import java.util.List;

public abstract class LiveListViewPagerAdapter extends FragmentStateAdapter {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getCommonLogger("AudienceViewPagerAdapter");

    private final List<LiveInfo> mLiveInfoList;

    private final FragmentManager     mFragmentManager;
    private final LiveInfoListService mLiveInfoListService;

    private boolean mIsDataLoaded;
    private boolean mIsLoading;

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

    @SuppressLint("NotifyDataSetChanged")
    public void retainOnlyFirstElement() {
        if (mLiveInfoList != null && mLiveInfoList.size() > 1) {
            mLiveInfoList.subList(1, mLiveInfoList.size()).clear();
            notifyDataSetChanged();
        }
    }

    public Fragment getFragment(int position) {
        return mFragmentManager.findFragmentByTag("f" + position);
    }

    public List<LiveInfo> getDataList() {
        return mLiveInfoList;
    }

    public void fetchData() {
        boolean disableRoomSliding = Boolean.TRUE.equals(AudienceContainerConfig.disableSliding.getValue());
        LOGGER.info("fetchLiveList disableRoomSliding:" + disableRoomSliding);
        if (disableRoomSliding) {
            return;
        }
        if (mIsLoading) {
            LOGGER.info("is start fetch data, waiting");
            return;
        }
        if (mIsDataLoaded && TextUtils.isEmpty(mLiveInfoListService.getLiveListDataCursor())) {
            LOGGER.info("there is no more data");
            return;
        }
        mIsLoading = true;
        mLiveInfoListService.fetchLiveList(new AudienceContainerViewDefine.LiveListCallback() {
            @Override
            public void onSuccess(String cursor, List<LiveInfo> liveInfoList) {
                boolean disableRoomSliding = Boolean.TRUE.equals(AudienceContainerConfig.disableSliding.getValue());
                if (!disableRoomSliding) {
                    int startPosition = mLiveInfoList.size();
                    mLiveInfoList.addAll(liveInfoList);
                    notifyItemRangeInserted(startPosition, mLiveInfoList.size());
                }
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
        boolean disableRoomSliding = Boolean.TRUE.equals(AudienceContainerConfig.disableSliding.getValue());
        LOGGER.info("refreshData disableRoomSliding:" + disableRoomSliding);
        if (disableRoomSliding) {
            if (callback != null) {
                callback.onComplete();
            }
            return;
        }
        mIsLoading = true;
        mLiveInfoListService.refreshLiveList(new AudienceContainerViewDefine.LiveListCallback() {
            @SuppressLint("NotifyDataSetChanged")
            @Override
            public void onSuccess(String cursor, List<LiveInfo> liveInfoList) {
                mLiveInfoList.clear();
                mLiveInfoList.addAll(liveInfoList);
                notifyDataSetChanged();
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

    @Override
    public int getItemCount() {
        return mLiveInfoList.size();
    }

    public abstract View onCreateView(LiveInfo liveInfo);

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

    private void addData(List<LiveInfo> liveInfoList) {
        int startPosition = mLiveInfoList.size();
        mLiveInfoList.addAll(liveInfoList);
        notifyItemRangeInserted(startPosition, mLiveInfoList.size());
    }

    public interface ActionCallback {
        void onComplete();
    }
}
