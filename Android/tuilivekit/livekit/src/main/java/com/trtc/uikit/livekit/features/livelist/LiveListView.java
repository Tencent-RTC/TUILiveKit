package com.trtc.uikit.livekit.features.livelist;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.fragment.app.FragmentActivity;

import com.trtc.uikit.livekit.features.livelist.LiveListViewDefine.LiveListViewAdapter;
import com.trtc.uikit.livekit.features.livelist.LiveListViewDefine.Style;
import com.trtc.uikit.livekit.features.livelist.access.DoubleColumnListViewAdapter;
import com.trtc.uikit.livekit.features.livelist.access.SingleColumnListViewAdapter;
import com.trtc.uikit.livekit.features.livelist.access.TUILiveListDataSource;
import com.trtc.uikit.livekit.features.livelist.manager.LiveInfoListService;
import com.trtc.uikit.livekit.features.livelist.view.doublecolumn.DoubleColumnListView;
import com.trtc.uikit.livekit.features.livelist.view.singlecolumn.SingleColumnListView;

public class LiveListView extends FrameLayout {
    private LiveListViewDefine.Style mStyle;
    private SingleColumnListView     mSingleColumnListView;
    private DoubleColumnListView     mDoubleColumnListView;

    private LiveInfoListService                  mLiveInfoListService;
    private FragmentActivity                     mFragmentActivity;
    private LiveListViewAdapter                  mLiveListViewAdapter;
    private LiveListViewDefine.OnItemClickListener mOnItemClickListener;
    private boolean                              mIsInit;

    public LiveListView(@NonNull Context context) {
        this(context, null, 0);
    }

    public LiveListView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public LiveListView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public void init(@NonNull FragmentActivity fragmentActivity, LiveListViewDefine.Style style) {
        init(fragmentActivity, style, null, null);
    }

    public void init(@NonNull FragmentActivity fragmentActivity, LiveListViewDefine.Style style,
                     LiveListViewAdapter adapter, LiveListViewDefine.LiveListDataSource dataSource) {
        mFragmentActivity = fragmentActivity;
        mStyle = style;
        mLiveListViewAdapter = adapter;
        if (dataSource == null) {
            dataSource = new TUILiveListDataSource();
        }
        mLiveInfoListService = new LiveInfoListService(dataSource);
        initLiveColumnListView(fragmentActivity, style, mLiveInfoListService, adapter);
        mIsInit = true;
    }

    public void updateColumnStyle(LiveListViewDefine.Style style) {
        mStyle = style;
        if (mIsInit) {
            initLiveColumnListView(mFragmentActivity, mStyle, mLiveInfoListService, mLiveListViewAdapter);
        }
    }

    public void setOnItemClickListener(LiveListViewDefine.OnItemClickListener listener) {
        mOnItemClickListener = listener;
        if (mStyle == Style.DOUBLE_COLUMN) {
            setDoubleColumnListViewClickLister(listener);
        } else {
            setSingleColumnListViewClickLister(listener);
        }
    }

    public void refreshData() {
        if (mStyle == Style.DOUBLE_COLUMN) {
            if (mDoubleColumnListView != null) {
                mDoubleColumnListView.refreshData();
            }
        } else {
            if (mSingleColumnListView != null) {
                mSingleColumnListView.refreshData();
            }
        }
    }

    private void initLiveColumnListView(@NonNull FragmentActivity fragmentActivity,
                                        @NonNull LiveListViewDefine.Style style,
                                        @NonNull LiveInfoListService liveInfoListService,
                                        LiveListViewAdapter adapter) {
        if (adapter == null) {
            if (style == Style.DOUBLE_COLUMN) {
                adapter = new DoubleColumnListViewAdapter(fragmentActivity);
            } else {
                adapter = new SingleColumnListViewAdapter(fragmentActivity);
            }
        }

        removeAllViews();
        if (style == Style.DOUBLE_COLUMN) {
            mDoubleColumnListView = new DoubleColumnListView(fragmentActivity);
            mDoubleColumnListView.init(fragmentActivity, adapter, liveInfoListService);
            addView(mDoubleColumnListView);
            setDoubleColumnListViewClickLister(mOnItemClickListener);
        } else {
            mSingleColumnListView = new SingleColumnListView(fragmentActivity);
            mSingleColumnListView.init(fragmentActivity, adapter, liveInfoListService);
            addView(mSingleColumnListView);
            setSingleColumnListViewClickLister(mOnItemClickListener);
        }
    }

    private void setDoubleColumnListViewClickLister(LiveListViewDefine.OnItemClickListener listener) {
        if (mDoubleColumnListView != null) {
            mDoubleColumnListView.setOnItemClickListener(listener);
        }
    }

    private void setSingleColumnListViewClickLister(LiveListViewDefine.OnItemClickListener listener) {
        if (mSingleColumnListView != null) {
            mSingleColumnListView.setOnItemClickListener(listener);
        }
    }
}
