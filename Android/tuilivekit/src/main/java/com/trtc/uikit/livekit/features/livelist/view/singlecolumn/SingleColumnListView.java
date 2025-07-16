package com.trtc.uikit.livekit.features.livelist.view.singlecolumn;

import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentActivity;
import androidx.lifecycle.Lifecycle;
import androidx.lifecycle.LifecycleObserver;
import androidx.lifecycle.Observer;
import androidx.lifecycle.OnLifecycleEvent;
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.pictureinpicture.PictureInPictureStore;
import com.trtc.uikit.livekit.features.livelist.LiveListViewDefine;
import com.trtc.uikit.livekit.features.livelist.manager.LiveInfoListService;
import com.trtc.uikit.livekit.features.livelist.view.liveListviewpager.LiveListViewPager;
import com.trtc.uikit.livekit.features.livelist.view.liveListviewpager.LiveListViewPagerAdapter;

import java.util.HashSet;

public class SingleColumnListView extends FrameLayout {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getComponentLogger("SingleColumnListView");

    private static final int REFRESH_TIME_INTERVAL = 1000;

    private final HashSet<SingleColumnItemView> mPlayStreamView = new HashSet<>(2);

    private FragmentActivity                       mFragmentActivity;
    private LiveListViewDefine.LiveListViewAdapter mLiveListViewAdapter;
    private LiveInfoListService                    mLiveInfoListService;

    private LiveListViewPager                      mLiveListViewPager;
    private LiveListViewPagerAdapter               mLiveListViewPagerAdapter;
    private SwipeRefreshLayout                     mSwipeRefreshLayout;
    private LiveListViewDefine.OnItemClickListener mOnItemClickListener;
    private SingleColumnItemView                   mWillEnterRoomView;
    private boolean                                mIsLoading;
    private boolean                                mIsResumed;
    private long                                   mLoadingTime;
    private boolean                                mIsInit;

    private final Observer<String> mPictureInPictureRoomIdObserver = this::onPictureInPictureRoomIdChanged;

    public SingleColumnListView(@NonNull Context context) {
        this(context, null, 0);
    }

    public SingleColumnListView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public SingleColumnListView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        LayoutInflater.from(getContext()).inflate(R.layout.livelist_single_column_list_view, this, true);
        initView();
    }

    private void initView() {
        mSwipeRefreshLayout = findViewById(R.id.swipe_layout);
        mSwipeRefreshLayout.setColorSchemeResources(R.color.common_design_standard_g5);
        mLiveListViewPager = findViewById(R.id.live_list_view_pager);
        mSwipeRefreshLayout.setOnRefreshListener(this::refreshData);
    }

    public void init(@NonNull FragmentActivity fragmentActivity, LiveListViewDefine.LiveListViewAdapter adapter,
                     LiveInfoListService liveInfoListService) {
        mFragmentActivity = fragmentActivity;
        mLiveListViewAdapter = adapter;
        mLiveInfoListService = liveInfoListService;
        mPlayStreamView.clear();
        mLiveListViewPagerAdapter = new LiveListViewPagerAdapter(mFragmentActivity,
                mLiveInfoListService) {
            @Override
            public View createLiveInfoView(TUILiveListManager.LiveInfo liveInfo) {
                SingleColumnItemView singleColumnItemView = new SingleColumnItemView(getContext());
                singleColumnItemView.createLiveInfoView(mLiveListViewAdapter, liveInfo);
                singleColumnItemView.setOnClickListener(view -> {
                    mWillEnterRoomView = (SingleColumnItemView) view;
                    if (mOnItemClickListener != null) {
                        mOnItemClickListener.onItemClick(view, liveInfo);
                    }
                });
                return singleColumnItemView;
            }

            @Override
            public void updateLiveInfoView(View view, TUILiveListManager.LiveInfo liveInfo) {
                SingleColumnItemView singleColumnItemView = (SingleColumnItemView) view;
                singleColumnItemView.updateLiveInfoView(liveInfo);
                singleColumnItemView.setOnClickListener(v -> {
                    mWillEnterRoomView = (SingleColumnItemView) v;
                    if (mOnItemClickListener != null) {
                        mOnItemClickListener.onItemClick(view, liveInfo);
                    }
                });
            }

            @Override
            public void onViewDidSlideIn(View view) {
                SingleColumnItemView itemView = (SingleColumnItemView) view;
                startPreviewLiveStream(itemView, false);
            }

            @Override
            public void onViewWillSlideIn(View view) {
                SingleColumnItemView itemView = (SingleColumnItemView) view;
                startPreviewLiveStream(itemView, true);
            }

            @Override
            public void onViewDidSlideOut(View view) {
                SingleColumnItemView itemView = (SingleColumnItemView) view;
                stopPreviewLiveStream(itemView);
            }

            @Override
            public void onViewSlideInCancelled(View view) {
                SingleColumnItemView itemView = (SingleColumnItemView) view;
                stopPreviewLiveStream(itemView);
            }
        };
        mLiveListViewPager.setAdapter(mLiveListViewPagerAdapter);
        mLiveListViewPagerAdapter.fetchData();
    }

    public void setOnItemClickListener(LiveListViewDefine.OnItemClickListener listener) {
        mOnItemClickListener = listener;
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        if (mFragmentActivity != null) {
            mFragmentActivity.getLifecycle().addObserver(mLifecycleObserver);
        }
        PictureInPictureStore.sharedInstance().getState().roomId.observeForever(mPictureInPictureRoomIdObserver);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        if (mFragmentActivity != null) {
            mFragmentActivity.getLifecycle().removeObserver(mLifecycleObserver);
        }
        stopAllPreviewLiveStream();
        PictureInPictureStore.sharedInstance().getState().roomId.removeObserver(mPictureInPictureRoomIdObserver);
    }

    private void startPreviewLiveStream(SingleColumnItemView itemView, boolean isMuteAudio) {
        itemView.startPreviewLiveStream(isMuteAudio);
        mPlayStreamView.add(itemView);
    }

    private void stopPreviewLiveStream(SingleColumnItemView itemView) {
        itemView.stopPreviewLiveStream();
        mPlayStreamView.remove(itemView);
    }

    private void stopAllPreviewLiveStream() {
        for (SingleColumnItemView itemView : mPlayStreamView) {
            itemView.stopPreviewLiveStream();
        }
        mPlayStreamView.clear();
    }

    private void pauseAllLiveStream() {
        for (SingleColumnItemView itemView : mPlayStreamView) {
            if (mWillEnterRoomView == itemView) {
                continue;
            }
            itemView.stopPreviewLiveStream();
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
        if (mLiveListViewPagerAdapter != null) {
            mLiveListViewPagerAdapter.refreshData(() -> {
                mSwipeRefreshLayout.setRefreshing(false);
                post(() -> {
                    mIsLoading = false;
                    if (!mIsResumed) {
                        LOGGER.info("the current activity is not resumed");
                        return;
                    }
                    stopAllPreviewLiveStream();
                    int currentPosition = mLiveListViewPager.getCurrentItem();
                    View currentView = mLiveListViewPager.findViewByPosition(currentPosition);
                    if (currentView != null) {
                        SingleColumnItemView columnItemView = (SingleColumnItemView) currentView;
                        startPreviewLiveStream(columnItemView, false);
                    }
                });
            });
        }
    }

    private void onPictureInPictureRoomIdChanged(String roomId) {
        if (TextUtils.isEmpty(roomId)) {
            for (SingleColumnItemView itemView : mPlayStreamView) {
                if (itemView.isPauseByPictureInPicture()) {
                    itemView.startPreviewLiveStream(false);
                }
            }
        }
    }

    private final LifecycleObserver mLifecycleObserver = new LifecycleObserver() {
        @OnLifecycleEvent(Lifecycle.Event.ON_RESUME)
        public void onResume() {
            mIsResumed = true;
            mWillEnterRoomView = null;
            if (mIsInit) {
                refreshData();
            } else {
                mIsInit = true;
            }
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
