package com.trtc.uikit.livekit.features.livelist.view.doublecolumn;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.fragment.app.FragmentActivity;
import androidx.lifecycle.Lifecycle;
import androidx.lifecycle.LifecycleObserver;
import androidx.lifecycle.OnLifecycleEvent;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.livelist.LiveListViewDefine;
import com.trtc.uikit.livekit.features.livelist.manager.LiveInfoListService;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@SuppressLint("ViewConstructor")
public class DoubleColumnListView extends FrameLayout {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getComponentLogger("DoubleColumnListView");

    private static final int REFRESH_TIME_INTERVAL = 1000;

    private final Context                       mContext;
    private final HashSet<DoubleColumnItemView> mPlayStreamView = new HashSet<>(2);

    private FragmentActivity                       mFragmentActivity;
    private LiveListViewDefine.LiveListViewAdapter mLiveListViewAdapter;
    private LiveInfoListService                    mLiveInfoListService;

    private SwipeRefreshLayout                     mSwipeRefreshLayout;
    private RecyclerView                           mRecyclerView;
    private DoubleColumnAdapter                    mAdapter;
    private GridLayoutManager                      mGridLayoutManager;
    private LiveListViewDefine.OnItemClickListener mOnItemClickListener;
    private DoubleColumnItemView                   mWillEnterRoomView;
    private boolean                                mIsLoading;
    private boolean                                mIsResumed;
    private long                                   mLoadingTime;

    public DoubleColumnListView(@NonNull Context context) {
        this(context, null, 0);
    }

    public DoubleColumnListView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public DoubleColumnListView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        LayoutInflater.from(mContext).inflate(R.layout.livelist_double_column_list_view, this, true);
        initView();
    }

    public void init(@NonNull FragmentActivity fragmentActivity, LiveListViewDefine.LiveListViewAdapter adapter,
                     LiveInfoListService liveInfoListService) {
        mFragmentActivity = fragmentActivity;
        mLiveListViewAdapter = adapter;
        mLiveInfoListService = liveInfoListService;
        initRecyclerView();
    }

    public void setOnItemClickListener(LiveListViewDefine.OnItemClickListener listener) {
        mOnItemClickListener = listener;
        if (mAdapter != null) {
            mAdapter.setOnItemClickListener(this::onLiveInfoViewClick);
        }
    }

    private void onLiveInfoViewClick(View view, TUILiveListManager.LiveInfo liveInfo) {
        mWillEnterRoomView = (DoubleColumnItemView) view;
        if (mOnItemClickListener != null) {
            mOnItemClickListener.onItemClick(view, liveInfo);
        }
    }

    private void initView() {
        mSwipeRefreshLayout = findViewById(R.id.swipe_layout);
        mRecyclerView = findViewById(R.id.recycler_view);
        mSwipeRefreshLayout.setColorSchemeResources(R.color.common_design_standard_g5);
        mGridLayoutManager = new GridLayoutManager(mContext, 2, GridLayoutManager.VERTICAL, false);
        mRecyclerView.setLayoutManager(mGridLayoutManager);
        mRecyclerView.addItemDecoration(new DoubleColumnAdapter.GridDividerItemDecoration(mContext));
        mSwipeRefreshLayout.setOnRefreshListener(this::refreshData);
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        if (mFragmentActivity != null) {
            mFragmentActivity.getLifecycle().addObserver(mLifecycleObserver);
        }
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        if (mFragmentActivity != null) {
            mFragmentActivity.getLifecycle().removeObserver(mLifecycleObserver);
        }
        stopAllPreviewLiveStream();
    }

    @SuppressLint("NotifyDataSetChanged")
    private void initRecyclerView() {
        mAdapter = new DoubleColumnAdapter(mFragmentActivity, mLiveListViewAdapter);
        mAdapter.setOnItemClickListener(this::onLiveInfoViewClick);
        mRecyclerView.setAdapter(mAdapter);
        mPlayStreamView.clear();
        mRecyclerView.addOnScrollListener(new RecyclerView.OnScrollListener() {
            private boolean isSlidingUpward;
            private boolean isScrolling;

            @Override
            public void onScrollStateChanged(@NonNull RecyclerView recyclerView, int newState) {
                if (newState == RecyclerView.SCROLL_STATE_IDLE) {
                    if (isSlidingUpward) {
                        int lastItemPosition = mGridLayoutManager.findLastVisibleItemPosition();
                        if (lastItemPosition == mAdapter.getItemCount() - 1
                                && !TextUtils.isEmpty(mLiveInfoListService.getLiveListDataCursor())) {
                            loadMoreData();
                        }
                    }
                    if (isScrolling) {
                        post(() -> autoPlayVideoStream());
                        isScrolling = false;
                    }
                }
            }

            @Override
            public void onScrolled(@NonNull RecyclerView recyclerView, int dx, int dy) {
                isSlidingUpward = dy > 0;
                isScrolling = true;
            }
        });
        List<TUILiveListManager.LiveInfo> liveInfoList = mLiveInfoListService.getLiveList();
        if (liveInfoList.isEmpty()) {
            refreshData();
        } else {
            mAdapter.setData(liveInfoList);
            mAdapter.notifyDataSetChanged();
            post(this::autoPlayVideoStream);
        }
    }

    private void autoPlayVideoStream() {
        LinearLayoutManager layoutManager = (LinearLayoutManager) mRecyclerView.getLayoutManager();
        if (layoutManager == null) {
            return;
        }
        int firstVisibleItemPosition = layoutManager.findFirstVisibleItemPosition();
        int lastVisibleItemPosition = layoutManager.findLastVisibleItemPosition();
        List<View> fullyVisibleItems = new ArrayList<>();
        for (int i = firstVisibleItemPosition; i <= lastVisibleItemPosition; i++) {
            View itemView = layoutManager.findViewByPosition(i);
            if (!(itemView instanceof DoubleColumnItemView)) {
                continue;
            }
            if (itemView.getTop() >= 0 && itemView.getBottom() <= mRecyclerView.getHeight()) {
                fullyVisibleItems.add(itemView);
            }
        }
        if (fullyVisibleItems.size() >= 2) {
            DoubleColumnItemView firstItem = (DoubleColumnItemView) (fullyVisibleItems.get(0));
            DoubleColumnItemView secondItem = (DoubleColumnItemView) (fullyVisibleItems.get(1));
            Set<DoubleColumnItemView> set = new HashSet<>();
            set.add(firstItem);
            set.add(secondItem);
            startPreviewLiveStream(set);
        } else if (fullyVisibleItems.size() == 1) {
            DoubleColumnItemView firstItem = (DoubleColumnItemView) (fullyVisibleItems.get(0));
            Set<DoubleColumnItemView> set = new HashSet<>();
            set.add(firstItem);
            startPreviewLiveStream(set);
        }
    }

    public void refreshData() {
        if (mIsLoading) {
            return;
        }

        if (System.currentTimeMillis() - mLoadingTime < REFRESH_TIME_INTERVAL) {
            removeCallbacks(mRunnable);
            postDelayed(mRunnable, REFRESH_TIME_INTERVAL);
            return;
        }

        mLoadingTime = System.currentTimeMillis();
        mIsLoading = true;
        if (mLiveInfoListService != null) {
            mLiveInfoListService.refreshLiveList(new LiveListViewDefine.LiveListCallback() {
                @SuppressLint("NotifyDataSetChanged")
                @Override
                public void onSuccess(String cursor, List<TUILiveListManager.LiveInfo> liveInfoList) {
                    mAdapter.setData(liveInfoList);
                    mAdapter.notifyDataSetChanged();
                    mSwipeRefreshLayout.setRefreshing(false);
                    post(() -> {
                        mIsLoading = false;
                        if (!mIsResumed) {
                            LOGGER.info("the current activity is not resumed");
                            return;
                        }
                        stopAllPreviewLiveStream();
                        autoPlayVideoStream();
                    });
                }

                @Override
                public void onError(int code, String message) {
                    LOGGER.error("refreshData failed:error,errorCode:" + code + ",message:" + message);
                    post(() -> {
                        mIsLoading = false;
                        mSwipeRefreshLayout.setRefreshing(false);
                    });
                }
            });
        }
    }

    private void loadMoreData() {
        if (mIsLoading) {
            return;
        }
        mAdapter.setLoadState(DoubleColumnAdapter.LOADING);
        mIsLoading = true;
        mLiveInfoListService.fetchLiveList(new LiveListViewDefine.LiveListCallback() {
            @Override
            public void onSuccess(String cursor, List<TUILiveListManager.LiveInfo> liveInfoList) {
                if (liveInfoList.isEmpty()) {
                    post(() -> {
                        mAdapter.setLoadState(DoubleColumnAdapter.LOADING_COMPLETE);
                        mIsLoading = false;
                    });
                    return;
                }

                post(() -> {
                    int itemCount = mAdapter.getItemCount();
                    mAdapter.addData(liveInfoList);
                    mAdapter.notifyItemRangeInserted(itemCount, itemCount + liveInfoList.size());
                    mIsLoading = false;
                    mAdapter.setLoadState(!TextUtils.isEmpty(cursor) ? DoubleColumnAdapter.LOADING_COMPLETE :
                            DoubleColumnAdapter.LOADING_END);
                });
            }

            @Override
            public void onError(int code, String message) {
                LOGGER.error("loadMoreData failed, errorCode:" + code + ",message:" + message);
                post(() -> mAdapter.setLoadState(DoubleColumnAdapter.LOADING_COMPLETE));
                mIsLoading = false;
            }
        });
    }

    private void startPreviewLiveStream(Set<DoubleColumnItemView> itemViewSet) {
        if (mPlayStreamView.equals(itemViewSet)) {
            for (DoubleColumnItemView itemView : itemViewSet) {
                itemView.startPreviewLiveStreamDelay();
            }
            return;
        }
        stopAllPreviewLiveStream();

        for (DoubleColumnItemView itemView : itemViewSet) {
            itemView.startPreviewLiveStreamDelay();
            mPlayStreamView.add(itemView);
        }
    }

    private void stopAllPreviewLiveStream() {
        for (DoubleColumnItemView itemView : mPlayStreamView) {
            itemView.stopPreviewLiveStream();
        }
        mPlayStreamView.clear();
    }

    private void resumeAllLiveStream() {
        for (DoubleColumnItemView itemView : mPlayStreamView) {
            itemView.startPreviewLiveStreamDelay();
        }
        mWillEnterRoomView = null;
    }

    private void pauseAllLiveStream() {
        for (DoubleColumnItemView itemView : mPlayStreamView) {
            if (mWillEnterRoomView == itemView) {
                continue;
            }
            itemView.stopPreviewLiveStream();
        }
    }

    private final LifecycleObserver mLifecycleObserver = new LifecycleObserver() {
        @OnLifecycleEvent(Lifecycle.Event.ON_RESUME)
        public void onResume() {
            mIsResumed = true;
            resumeAllLiveStream();
        }

        @OnLifecycleEvent(Lifecycle.Event.ON_PAUSE)
        public void onPause() {
            mIsResumed = false;
            pauseAllLiveStream();
        }
    };

    private final Runnable mRunnable = new Runnable() {
        @Override
        public void run() {
            if (!mIsLoading) {
                mSwipeRefreshLayout.setRefreshing(false);
            }
        }
    };
}

