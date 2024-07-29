package com.trtc.uikit.livekit.common.uicomponent.roomlist.view.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ProgressBar;
import android.widget.TextView;

import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.uikit.livekit.R;

public class LoadMoreAdapterWrapper extends RecyclerView.Adapter<RecyclerView.ViewHolder> {

    private static final int TYPE_ITEM   = 1;
    private static final int TYPE_FOOTER = 2;

    public static final int LOADING          = 1;
    public static final int LOADING_COMPLETE = 2;
    public static final int LOADING_END      = 3;

    private       int                  loadState = LOADING_COMPLETE;
    private final RecyclerView.Adapter adapter;

    public LoadMoreAdapterWrapper(RecyclerView.Adapter adapter) {
        this.adapter = adapter;
    }

    @Override
    public int getItemViewType(int position) {
        if (position + 1 == getItemCount()) {
            return TYPE_FOOTER;
        } else {
            return TYPE_ITEM;
        }
    }

    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        if (viewType == TYPE_FOOTER) {
            View view = LayoutInflater.from(parent.getContext())
                    .inflate(R.layout.livekit_live_room_list_refresh_footer, parent, false);
            return new FootViewHolder(view);
        } else {
            return adapter.onCreateViewHolder(parent, viewType);
        }
    }

    @Override
    public void onBindViewHolder(RecyclerView.ViewHolder holder, int position) {
        if (holder instanceof FootViewHolder) {
            FootViewHolder footViewHolder = (FootViewHolder) holder;
            if (adapter.getItemCount() == 0) {
                footViewHolder.tvNoMoreData.setText(R.string.livekit_no_room_tip);
            } else {
                footViewHolder.tvNoMoreData.setText(R.string.livekit_no_more_data);
            }
            switch (loadState) {
                case LOADING:
                    footViewHolder.pbProgressBar.setVisibility(View.VISIBLE);
                    footViewHolder.tvLoading.setVisibility(View.VISIBLE);
                    footViewHolder.tvNoMoreData.setVisibility(View.GONE);
                    break;
                case LOADING_COMPLETE:
                    footViewHolder.pbProgressBar.setVisibility(View.INVISIBLE);
                    footViewHolder.tvLoading.setVisibility(View.INVISIBLE);
                    footViewHolder.tvNoMoreData.setVisibility(View.GONE);
                    break;
                case LOADING_END:
                    footViewHolder.pbProgressBar.setVisibility(View.GONE);
                    footViewHolder.tvLoading.setVisibility(View.GONE);
                    footViewHolder.tvNoMoreData.setVisibility(View.VISIBLE);
                    break;
                default:
                    break;
            }
        } else {
            adapter.onBindViewHolder(holder, position);
        }
    }

    @Override
    public int getItemCount() {
        return adapter.getItemCount() + 1;
    }

    @Override
    public void onAttachedToRecyclerView(RecyclerView recyclerView) {
        super.onAttachedToRecyclerView(recyclerView);
        RecyclerView.LayoutManager manager = recyclerView.getLayoutManager();
        if (manager instanceof GridLayoutManager) {
            final GridLayoutManager gridManager = ((GridLayoutManager) manager);
            gridManager.setSpanSizeLookup(new GridLayoutManager.SpanSizeLookup() {
                @Override
                public int getSpanSize(int position) {
                    return getItemViewType(position) == TYPE_FOOTER ? gridManager.getSpanCount() : 1;
                }
            });
        }
    }

    private class FootViewHolder extends RecyclerView.ViewHolder {

        ProgressBar pbProgressBar;
        TextView    tvLoading;
        TextView    tvNoMoreData;

        FootViewHolder(View itemView) {
            super(itemView);
            pbProgressBar = itemView.findViewById(R.id.pb_progress_bar);
            tvLoading = itemView.findViewById(R.id.tv_loading_data);
            tvNoMoreData = itemView.findViewById(R.id.tv_no_more_data);
        }
    }

    public void setLoadState(int loadState) {
        this.loadState = loadState;
        notifyDataSetChanged();
    }
}