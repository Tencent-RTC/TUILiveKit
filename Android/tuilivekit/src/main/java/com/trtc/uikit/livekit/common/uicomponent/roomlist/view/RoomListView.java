package com.trtc.uikit.livekit.common.uicomponent.roomlist.view;

import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.roomlist.service.RoomListService;
import com.trtc.uikit.livekit.common.uicomponent.roomlist.store.RoomListState;
import com.trtc.uikit.livekit.common.uicomponent.roomlist.view.adapter.LoadMoreAdapterWrapper;
import com.trtc.uikit.livekit.common.uicomponent.roomlist.view.adapter.RoomListAdapter;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;

public class RoomListView extends BasicView {
    private SwipeRefreshLayout     mSwipeRefreshLayout;
    private RecyclerView           mRecyclerView;
    private LoadMoreAdapterWrapper mAdapter;

    private final RoomListService mRoomListService = new RoomListService();
    private final RoomListState   mRoomListState   = mRoomListService.mRoomListState;

    private final Observer<Boolean> mRefreshStatusObserver = refreshing ->
            post(() -> mSwipeRefreshLayout.setRefreshing(refreshing));

    private final Observer<Boolean> mLoadStatusObserver = loading ->
            post(() -> {
                if (loading) {
                    mAdapter.setLoadState((LoadMoreAdapterWrapper.LOADING));
                } else {
                    mAdapter.setLoadState(!TextUtils.isEmpty(mRoomListState.mFetchListCursor)
                            ? LoadMoreAdapterWrapper.LOADING_COMPLETE
                            : LoadMoreAdapterWrapper.LOADING_END);
                }
            });

    public RoomListView(@NonNull Context context, @NonNull LiveController liveController) {
        super(context, liveController);
    }

    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_live_room_list_view, this, true);

        initSwipeRefreshLayout();
        initRecyclerView();
        refreshFetch();
    }

    private void refreshFetch() {
        mRoomListService.refreshFetchList();
    }

    @Override
    protected void addObserver() {
        mRoomListState.mRefreshStatus.observe(mRefreshStatusObserver);
        mRoomListState.mLoadStatus.observe(mLoadStatusObserver);
    }

    @Override
    protected void removeObserver() {
        mRoomListState.mRefreshStatus.removeObserver(mRefreshStatusObserver);
        mRoomListState.mLoadStatus.removeObserver(mLoadStatusObserver);
    }

    private void initSwipeRefreshLayout() {
        mSwipeRefreshLayout = findViewById(R.id.sfl_cover);
        mSwipeRefreshLayout.setColorSchemeResources(R.color.livekit_design_standard_g5);
        mSwipeRefreshLayout.setOnRefreshListener(this::refreshFetch);
    }

    private void initRecyclerView() {
        mRecyclerView = findViewById(R.id.rv_cover);

        GridLayoutManager layoutManager = new GridLayoutManager(mContext, 2, GridLayoutManager.VERTICAL, false);
        mRecyclerView.setLayoutManager(layoutManager);
        mRecyclerView.addItemDecoration(new RoomListAdapter.GridDividerItemDecoration(mContext));
        RoomListAdapter adapter = new RoomListAdapter(mContext, mRoomListState.mLiveList.get());
        mAdapter = new LoadMoreAdapterWrapper(adapter);
        mRecyclerView.setAdapter(mAdapter);
        mRecyclerView.addOnScrollListener(new RecyclerView.OnScrollListener() {
            private boolean isSlidingUpward = false;

            @Override
            public void onScrollStateChanged(@NonNull RecyclerView recyclerView, int newState) {
                if (newState == RecyclerView.SCROLL_STATE_IDLE && isSlidingUpward) {
                    int lastItemPosition = layoutManager.findLastVisibleItemPosition();
                    if (lastItemPosition == mAdapter.getItemCount() - 1 && isSlidingUpward) {
                        mRecyclerView.post(() -> mAdapter.setLoadState(LoadMoreAdapterWrapper.LOADING));
                        mRoomListService.fetchLiveList(false);
                    }
                }
            }

            @Override
            public void onScrolled(@NonNull RecyclerView recyclerView, int dx, int dy) {
                isSlidingUpward = dy > 0;
            }
        });
    }
}

