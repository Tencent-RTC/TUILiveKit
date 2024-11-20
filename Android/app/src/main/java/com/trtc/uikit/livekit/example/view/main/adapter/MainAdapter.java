package com.trtc.uikit.livekit.example.view.main.adapter;

import android.content.Context;
import android.content.Intent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.uikit.livekit.example.R;
import com.trtc.uikit.livekit.example.view.main.model.MainItemData;
import com.trtc.uikit.livekit.example.view.scene.VideoLiveActivity;
import com.trtc.uikit.livekit.example.view.scene.VoiceRoomActivity;

import java.util.List;

public class MainAdapter extends RecyclerView.Adapter<MainAdapter.ItemViewHolder> {

    private final Context            mContext;
    private final List<MainItemData> mDataList;

    public MainAdapter(Context context, List<MainItemData> itemDataList) {
        this.mContext = context;
        this.mDataList = itemDataList;
    }

    @NonNull
    @Override
    public MainAdapter.ItemViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.app_adapter_main_item, parent, false);
        return new ItemViewHolder(view, mContext);
    }

    @Override
    public void onBindViewHolder(@NonNull ItemViewHolder holder, int position) {
        MainItemData item = mDataList.get(position);
        holder.bind(item);
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    @Override
    public int getItemCount() {
        return mDataList.size();
    }


    public static class ItemViewHolder extends RecyclerView.ViewHolder {

        private final Context   mContext;
        private final ImageView mImageIcon;
        private final TextView  mTextTitle;
        private final TextView  mTextSubTitle;

        public ItemViewHolder(@NonNull View itemView, Context context) {
            super(itemView);
            mContext = context;
            mImageIcon = itemView.findViewById(R.id.iv_icon);
            mTextTitle = itemView.findViewById(R.id.tv_title);
            mTextSubTitle = itemView.findViewById(R.id.tv_subtitle);
        }

        public void bind(MainItemData mainItemData) {
            if (mainItemData == null) {
                return;
            }
            itemView.setOnClickListener(v -> {
                switch (mainItemData.getType()) {
                    case TYPE_VOICE_ROOM:
                        startVoiceRoomActivity();
                        break;
                    case TYPE_VIDEO_LIVE:
                    default:
                        startVideoLiveActivity();
                        break;
                }
            });
            mTextTitle.setText(mainItemData.getTitle());
            mImageIcon.setImageResource(mainItemData.getResId());
            mTextSubTitle.setText(mainItemData.getSubTitle());
        }

        private void startVideoLiveActivity() {
            Intent intent = new Intent(mContext, VideoLiveActivity.class);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            mContext.startActivity(intent);
        }

        private void startVoiceRoomActivity() {
            Intent intent = new Intent(mContext, VoiceRoomActivity.class);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            mContext.startActivity(intent);
        }
    }

}
