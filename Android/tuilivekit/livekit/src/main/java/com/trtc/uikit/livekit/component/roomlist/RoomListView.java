package com.trtc.uikit.livekit.component.roomlist;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.lifecycle.Observer;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.roomlist.service.RoomListService;
import com.trtc.uikit.livekit.component.roomlist.store.RoomListState;
import com.trtc.uikit.livekit.component.roomlist.view.adapter.LoadMoreAdapterWrapper;
import com.trtc.uikit.livekit.component.roomlist.view.adapter.RoomListAdapter;

@SuppressLint("ViewConstructor")
public class RoomListView extends FrameLayout {
    private       SwipeRefreshLayout     mSwipeRefreshLayout;
    private       RecyclerView           mRecyclerView;
    private       LoadMoreAdapterWrapper mAdapter;
    private final Context                mContext;
    private final RoomListService        mRoomListService       = new RoomListService();
    private final RoomListState          mRoomListState         = mRoomListService.mRoomListState;
    private final Observer<Boolean>      mRefreshStatusObserver = this::onRefresh;
    private final Observer<Boolean>      mLoadStatusObserver    = this::onLoadMore;

    public RoomListView(@NonNull Context context) {
        super(context);
        mContext = context;
        LayoutInflater.from(mContext).inflate(R.layout.livekit_live_room_list_view, this, true);
    }

    public RoomListView(Context context, AttributeSet attrs) {
        super(context, attrs);
        mContext = context;
        LayoutInflater.from(mContext).inflate(R.layout.livekit_live_room_list_view, this, true);
    }

    public RoomListView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        LayoutInflater.from(mContext).inflate(R.layout.livekit_live_room_list_view, this, true);
    }

    protected void initView() {
        initSwipeRefreshLayout();
        initRecyclerView();
    }

    protected void refreshRoomList() {
        mRoomListService.refreshFetchList();
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        initView();
        addObserver();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void addObserver() {
        mRoomListState.mRefreshStatus.observeForever(mRefreshStatusObserver);
        mRoomListState.mLoadStatus.observeForever(mLoadStatusObserver);
    }


    private void removeObserver() {
        mRoomListState.mRefreshStatus.removeObserver(mRefreshStatusObserver);
        mRoomListState.mLoadStatus.removeObserver(mLoadStatusObserver);
    }

    private void initSwipeRefreshLayout() {
        mSwipeRefreshLayout = findViewById(R.id.sfl_cover);
        mSwipeRefreshLayout.setColorSchemeResources(R.color.livekit_design_standard_g5);
        mSwipeRefreshLayout.setOnRefreshListener(this::refreshRoomList);
    }

    private void initRecyclerView() {
        mRecyclerView = findViewById(R.id.rv_cover);

        GridLayoutManager layoutManager = new GridLayoutManager(mContext, 2, GridLayoutManager.VERTICAL, false);
        mRecyclerView.setLayoutManager(layoutManager);
        mRecyclerView.addItemDecoration(new RoomListAdapter.GridDividerItemDecoration(mContext));
        RoomListAdapter adapter = new RoomListAdapter(mContext, mRoomListState.mLiveList.getValue());
        mAdapter = new LoadMoreAdapterWrapper(adapter);
        mRecyclerView.setAdapter(mAdapter);
        mRecyclerView.addOnScrollListener(new RecyclerView.OnScrollListener() {
            private boolean isSlidingUpward = false;

            @Override
            public void onScrollStateChanged(@NonNull RecyclerView recyclerView, int newState) {
                if (newState == RecyclerView.SCROLL_STATE_IDLE && isSlidingUpward) {
                    int lastItemPosition = layoutManager.findLastVisibleItemPosition();
                    if (lastItemPosition == mAdapter.getItemCount() - 1
                            && !TextUtils.isEmpty(mRoomListState.mFetchListCursor)) {
                        mRecyclerView.post(() -> mAdapter.setLoadState(LoadMoreAdapterWrapper.LOADING));
                        mRoomListService.fetchLiveList(false, null);
                    }
                }
            }

            @Override
            public void onScrolled(@NonNull RecyclerView recyclerView, int dx, int dy) {
                isSlidingUpward = dy > 0;
            }
        });
    }

    private void onRefresh(Boolean refreshing) {
        post(() -> mSwipeRefreshLayout.setRefreshing(refreshing));
    }

    private void onLoadMore(Boolean loading) {
        post(() -> {
            if (loading) {
                mAdapter.setLoadState((LoadMoreAdapterWrapper.LOADING));
            } else {
                mAdapter.setLoadState(!TextUtils.isEmpty(mRoomListState.mFetchListCursor)
                        ? LoadMoreAdapterWrapper.LOADING_COMPLETE
                        : LoadMoreAdapterWrapper.LOADING_END);
            }
        });
    }
}

