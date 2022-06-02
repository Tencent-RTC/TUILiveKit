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
import com.tencent.qcloud.tuikit.tuiaudioeffect.model.VoiceItemEntity;

import java.util.List;

import de.hdodenhof.circleimageview.CircleImageView;

/**
 * 变声/混响选择RecyclerView
 */
public class VoiceRecyclerView extends RecyclerView {

    private static final String TAG = "VoiceRecyclerView";

    public VoiceRecyclerView(@NonNull Context context) {
        super(context);
        init();
    }

    public VoiceRecyclerView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    private void init() {
        LinearLayoutManager reverbLayoutManager = new LinearLayoutManager(getContext());
        reverbLayoutManager.setOrientation(LinearLayoutManager.HORIZONTAL);
        setLayoutManager(reverbLayoutManager);
    }

    public void setSelectPosition(int position) {
        RecyclerViewAdapter adapter = (RecyclerViewAdapter) getAdapter();
        if (null != adapter) {
            adapter.setSelectPosition(position);
            Log.d(TAG, "setSelectPosition:" + position);
        }
    }

    public static class RecyclerViewAdapter extends Adapter<ViewHolder> {

        private List<VoiceItemEntity> mItemList;
        private OnItemClickListener   mOnItemClickListener;
        private int                   mSelectPosition = 0;

        public RecyclerViewAdapter(List<VoiceItemEntity> list, OnItemClickListener listener) {
            this.mItemList = list;
            this.mOnItemClickListener = listener;
        }

        public class ViewHolder extends RecyclerView.ViewHolder {
            private CircleImageView mImageMusic;
            private TextView        mTextTitle;
            private Context         mContext;

            public ViewHolder(View itemView) {
                super(itemView);
                initView(itemView);
                mContext = itemView.getContext();
            }

            public void bind(final VoiceItemEntity model, final int position, final OnItemClickListener listener) {
                mImageMusic.setImageResource(model.mIconId);
                mTextTitle.setText(model.mTitle);
                if (getSelectPosition() == position) {
                    mImageMusic.setImageResource(model.mSelectIconId);
                    mTextTitle.setTextColor(mContext.getResources().getColor(R.color.tuiaudioeffect_color_blue));
                } else {
                    mImageMusic.setImageResource(model.mIconId);
                    mTextTitle.setTextColor(mContext.getResources().getColor(R.color.tuiaudioeffect_dark_black));
                }
                itemView.setOnClickListener(new OnClickListener() {
                    @Override
                    public void onClick(View v) {
                        listener.onItemClick(position);
                    }
                });
            }

            private void initView(final View itemView) {
                mImageMusic = itemView.findViewById(R.id.img_item);
                mTextTitle = itemView.findViewById(R.id.tv_title);
            }
        }

        @Override
        public ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
            Context context = parent.getContext();
            LayoutInflater inflater = LayoutInflater.from(context);
            View view = inflater.inflate(R.layout.tuiaudioeffect_main_entry_item, parent, false);
            ViewHolder viewHolder = new ViewHolder(view);
            return viewHolder;
        }

        @Override
        public void onBindViewHolder(RecyclerView.ViewHolder holder, final int position) {
            VoiceItemEntity item = mItemList.get(position);
            ((ViewHolder) holder).bind(item, position, new OnItemClickListener() {
                @Override
                public void onItemClick(int position) {
                    if (getSelectPosition() != position) {
                        setSelectPosition(position);
                        if (null != mOnItemClickListener) {
                            mOnItemClickListener.onItemClick(position);
                        }
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

        public void setSelectPosition(int position) {
            mSelectPosition = position;
            notifyDataSetChanged();
        }

        public VoiceItemEntity getItem(int position) {
            if (position >= 0 && position < mItemList.size()) {
                return mItemList.get(position);
            }
            Log.e(TAG, "position is invalid");
            return null;
        }
    }

    public interface OnItemClickListener {
        void onItemClick(int position);
    }

}
