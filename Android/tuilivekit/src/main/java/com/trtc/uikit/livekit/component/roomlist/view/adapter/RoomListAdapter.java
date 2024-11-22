package com.trtc.uikit.livekit.component.roomlist.view.adapter;

import android.content.Context;
import android.content.Intent;
import android.graphics.Rect;
import android.text.TextUtils;
import android.util.DisplayMetrics;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;
import com.trtc.uikit.livekit.component.roomlist.view.ListAudienceActivity;
import com.trtc.uikit.livekit.livestream.view.anchor.VideoLiveAnchorActivity;

import java.util.List;
import java.util.Objects;

public class RoomListAdapter extends RecyclerView.Adapter<RoomListAdapter.ViewHolder> {
    private final Context        mContext;
    private final List<LiveInfo> mDataList;

    public RoomListAdapter(Context context, List<LiveInfo> dataList) {
        mContext = context;
        mDataList = dataList;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.livekit_adapter_item_room_list,
                parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        LiveInfo liveInfo = mDataList.get(position);
        TUIRoomDefine.RoomInfo roomInfo = liveInfo.roomInfo;
        ImageLoader.load(mContext, holder.mImageCover, liveInfo.coverUrl, R.drawable.livekit_live_stream_default_cover);
        ImageLoader.load(mContext, holder.mImageAvatar, roomInfo.ownerAvatarUrl,
                R.drawable.livekit_live_stream_default_cover);
        holder.mTextRoomName.setText(TextUtils.isEmpty(roomInfo.name) ? roomInfo.roomId : roomInfo.name);
        holder.mTextAnchorName.setText(TextUtils.isEmpty(roomInfo.ownerName) ? roomInfo.ownerId : roomInfo.ownerName);
        holder.mTextAudienceCountInfo.setText(
                mContext.getString(R.string.livekit_audience_count_in_room, liveInfo.viewCount));
        holder.mLayoutCoverBorder.setTag(liveInfo);
        holder.mLayoutCoverBorder.setOnClickListener((view) -> {
            FloatWindowManager.getInstance().releaseFloatWindow();
            final LiveInfo info = (LiveInfo) view.getTag();
            if (info.roomInfo != null && Objects.equals(info.roomInfo.ownerId, TUILogin.getUserId())
                    && !info.roomInfo.roomId.startsWith("voice_")) {
                Intent intent = new Intent(mContext, VideoLiveAnchorActivity.class);
                intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                intent.putExtra(VideoLiveAnchorActivity.INTENT_KEY_ROOM_ID, info.roomInfo.roomId);
                intent.putExtra(VideoLiveAnchorActivity.INTENT_KEY_NEED_CREATE, false);
                mContext.startActivity(intent);
            } else {
                Intent intent = new Intent(mContext, ListAudienceActivity.class);
                intent.putExtra("roomId", info.roomInfo.roomId);
                mContext.startActivity(intent);
            }
        });
    }

    @Override
    public int getItemCount() {
        return mDataList.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        private final ImageView   mImageCover;
        private final ImageView   mImageAvatar;
        private final TextView    mTextRoomName;
        private final TextView    mTextAnchorName;
        private final TextView    mTextAudienceCountInfo;
        private final FrameLayout mLayoutCoverBorder;

        public ViewHolder(@NonNull View itemView) {
            super(itemView);
            mTextRoomName = itemView.findViewById(R.id.tv_room_name);
            mTextAnchorName = itemView.findViewById(R.id.tv_anchor_name);
            mImageCover = itemView.findViewById(R.id.iv_cover);
            mImageAvatar = itemView.findViewById(R.id.iv_avatar);
            mTextAudienceCountInfo = itemView.findViewById(R.id.tv_audience_count_info);
            mLayoutCoverBorder = itemView.findViewById(R.id.fl_cover_border);
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
}