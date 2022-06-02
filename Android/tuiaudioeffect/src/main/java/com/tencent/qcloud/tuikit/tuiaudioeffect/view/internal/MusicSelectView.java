package com.tencent.qcloud.tuikit.tuiaudioeffect.view.internal;

import android.content.Context;
import android.util.AttributeSet;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.qcloud.tuikit.tuiaudioeffect.R;
import com.tencent.qcloud.tuikit.tuiaudioeffect.model.BGMItemEntity;

import java.util.List;

/**
 * 歌曲选择滑动条
 * 取值范围：[0, 100]
 */
public class MusicSelectView extends RecyclerView {

    private static final String TAG = "MusicSelectView";

    public MusicSelectView(@NonNull Context context) {
        super(context);
        init();
    }

    public MusicSelectView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    private void init() {
        LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        setLayoutManager(layoutManager);
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        private TextView mTextTitle;

        public ViewHolder(View itemView) {
            super(itemView);
            initView(itemView);
        }

        public void bind(final BGMItemEntity model, final int position, final OnItemClickListener listener) {
            mTextTitle.setText(model.mTitle);
            itemView.setOnClickListener(new OnClickListener() {
                @Override
                public void onClick(View v) {
                    if (null != listener) {
                        listener.onItemClick(position);
                    }
                }
            });
        }

        private void initView(final View itemView) {
            mTextTitle = itemView.findViewById(R.id.tv_bgm_title);
        }
    }


    public static class RecyclerViewAdapter extends Adapter<RecyclerView.ViewHolder> {

        private List<BGMItemEntity> mItemList;
        private OnItemClickListener mOnItemClickListener;
        private int                 mSelectPosition = 0;

        public RecyclerViewAdapter(List<BGMItemEntity> list, OnItemClickListener listener) {
            this.mItemList = list;
            this.mOnItemClickListener = listener;
        }

        @Override
        public RecyclerView.ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
            Context context = parent.getContext();
            LayoutInflater inflater = LayoutInflater.from(context);
            View view = inflater.inflate(R.layout.tuiaudioeffect_bgm_entry_item, parent, false);
            ViewHolder viewHolder = new ViewHolder(view);
            return viewHolder;
        }

        @Override
        public void onBindViewHolder(RecyclerView.ViewHolder holder, final int position) {
            ((ViewHolder) holder).bind(mItemList.get(position), position, new OnItemClickListener() {
                @Override
                public void onItemClick(int position) {
                    mSelectPosition = position;
                    if (null != mOnItemClickListener) {
                        mOnItemClickListener.onItemClick(position);
                    }
                }
            });
        }

        @Override
        public int getItemCount() {
            return mItemList.size();
        }

        public int getSelectPosition() {
            return mSelectPosition;
        }

        public BGMItemEntity getItem(int position) {
            if (position >= 0 && position < mItemList.size()) {
                return mItemList.get(position);
            }
            Log.e(TAG, "position is invalid");
            return null;
        }

        public void setOnItemClickListener(OnItemClickListener listener) {
            this.mOnItemClickListener = listener;
        }
    }


    public interface OnItemClickListener {
        void onItemClick(int position);
    }
}
