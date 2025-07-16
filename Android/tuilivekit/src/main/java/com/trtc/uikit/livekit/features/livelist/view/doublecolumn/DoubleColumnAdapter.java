package com.trtc.uikit.livekit.features.livelist.view.doublecolumn;

import android.content.Context;
import android.graphics.Rect;
import android.util.DisplayMetrics;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ProgressBar;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.features.livelist.LiveListViewDefine;

import java.util.ArrayList;
import java.util.List;

public class DoubleColumnAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {
    private static final int TYPE_ITEM   = 1;
    private static final int TYPE_FOOTER = 2;

    public static final int LOADING          = 1;
    public static final int LOADING_COMPLETE = 2;
    public static final int LOADING_END      = 3;

    private       int                                    mLoadState = LOADING_COMPLETE;
    private final List<LiveInfo>                         mLiveInfoList;
    private final LiveListViewDefine.LiveListViewAdapter mLiveListViewAdapter;
    private final Context                                mContext;
    private       LiveListViewDefine.OnItemClickListener   mOnItemClickListener;

    public DoubleColumnAdapter(Context context, LiveListViewDefine.LiveListViewAdapter adapter) {
        mContext = context;
        mLiveListViewAdapter = adapter;
        mLiveInfoList = new ArrayList<>();
    }

    public void addData(List<LiveInfo> liveInfoList) {
        mLiveInfoList.addAll(liveInfoList);
    }

    public void setData(List<LiveInfo> liveInfoList) {
        mLiveInfoList.clear();
        mLiveInfoList.addAll(liveInfoList);
    }

    @Override
    public int getItemViewType(int position) {
        if (position + 1 == getItemCount()) {
            return TYPE_FOOTER;
        } else {
            return TYPE_ITEM;
        }
    }

    @NonNull
    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        if (viewType == TYPE_FOOTER) {
            View view = LayoutInflater.from(mContext).inflate(R.layout.livelist_layout_refresh_footer, parent, false);
            return new FootViewHolder(view);
        } else {
            DoubleColumnItemView liveListItemView = new DoubleColumnItemView(mContext);
            return new LiveInfoViewHolder(liveListItemView);
        }
    }

    @Override
    public void onBindViewHolder(@NonNull RecyclerView.ViewHolder holder, int position) {
        if (holder instanceof FootViewHolder) {
            FootViewHolder footViewHolder = (FootViewHolder) holder;
            bindFootViewHolder(footViewHolder);
        } else if (holder instanceof LiveInfoViewHolder) {
            LiveInfoViewHolder liveInfoViewHolder = (LiveInfoViewHolder) holder;
            bindLiveInfoViewHolder(liveInfoViewHolder, position);
        }
    }

    @Override
    public int getItemCount() {
        return mLiveInfoList.size() + 1;
    }

    @Override
    public void onAttachedToRecyclerView(@NonNull RecyclerView recyclerView) {
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

    public void setOnItemClickListener(LiveListViewDefine.OnItemClickListener listener) {
        mOnItemClickListener = listener;
    }

    private void bindFootViewHolder(FootViewHolder holder) {
        if (mLiveInfoList.isEmpty()) {
            holder.tvNoMoreData.setText(R.string.livelist_empty_data);
        } else {
            holder.tvNoMoreData.setText(R.string.livelist_no_more_data);
        }
        switch (mLoadState) {
            case LOADING:
                holder.pbProgressBar.setVisibility(View.VISIBLE);
                holder.tvLoading.setVisibility(View.VISIBLE);
                holder.tvNoMoreData.setVisibility(View.GONE);
                break;
            case LOADING_COMPLETE:
                holder.pbProgressBar.setVisibility(View.INVISIBLE);
                holder.tvLoading.setVisibility(View.INVISIBLE);
                holder.tvNoMoreData.setVisibility(View.GONE);
                break;
            case LOADING_END:
                holder.pbProgressBar.setVisibility(View.GONE);
                holder.tvLoading.setVisibility(View.GONE);
                holder.tvNoMoreData.setVisibility(View.VISIBLE);
                break;
            default:
                break;
        }
    }

    private void bindLiveInfoViewHolder(LiveInfoViewHolder holder, int position) {
        LiveInfo liveInfo = mLiveInfoList.get(position);
        DoubleColumnItemView liveListItemView = (DoubleColumnItemView) holder.itemView;
        if (holder.liveInfo == null) {
            liveListItemView.createLiveInfoView(mLiveListViewAdapter, liveInfo);
        } else {
            liveListItemView.updateLiveInfoView(liveInfo);
        }
        holder.bind(liveInfo);
        holder.itemView.setTag(liveInfo);
        holder.itemView.setEnabled(true);
        holder.itemView.setOnClickListener((view) -> {
            if (mOnItemClickListener != null) {
                mOnItemClickListener.onItemClick(view, (TUILiveListManager.LiveInfo) view.getTag());
            }
        });
    }

    public static class LiveInfoViewHolder extends RecyclerView.ViewHolder {
        private LiveInfo liveInfo;

        public LiveInfoViewHolder(@NonNull View itemView) {
            super(itemView);
        }

        public void bind(LiveInfo liveInfo) {
            this.liveInfo = liveInfo;
        }
    }

    public static class GridDividerItemDecoration extends RecyclerView.ItemDecoration {
        private final int mDividerValue;

        public GridDividerItemDecoration(Context context) {
            DisplayMetrics metrics = context.getResources().getDisplayMetrics();
            mDividerValue = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, 3, metrics);
        }

        @Override
        public void getItemOffsets(Rect outRect, @NonNull View view, @NonNull RecyclerView parent,
                                   @NonNull RecyclerView.State state) {
            outRect.top = mDividerValue;
            outRect.left = mDividerValue;
            outRect.right = mDividerValue;
            outRect.bottom = mDividerValue;
        }
    }

    private static class FootViewHolder extends RecyclerView.ViewHolder {
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
        mLoadState = loadState;
        notifyItemChanged(getItemCount() - 1);
    }
}