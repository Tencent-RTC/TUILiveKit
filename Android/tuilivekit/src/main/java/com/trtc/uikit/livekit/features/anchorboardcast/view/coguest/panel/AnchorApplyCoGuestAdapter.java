package com.trtc.uikit.livekit.features.anchorboardcast.view.coguest.panel;

import static com.tencent.cloud.tuikit.engine.common.TUICommonDefine.Error.ALL_SEAT_OCCUPIED;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.constraintlayout.utils.widget.ImageFilterView;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;

import java.util.concurrent.CopyOnWriteArrayList;

import io.trtc.tuikit.atomicxcore.api.LiveCoreView;

public class AnchorApplyCoGuestAdapter extends RecyclerView.Adapter<AnchorApplyCoGuestAdapter.ApplyLinkMicViewHolder> {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("AnchorApplyCoGuestAdapter");

    private final AnchorManager                                mLiveManage;
    private final LiveCoreView                                 mLiveStream;
    private final Context                                      mContext;
    private final CopyOnWriteArrayList<TUIRoomDefine.UserInfo> mData = new CopyOnWriteArrayList<>();

    public AnchorApplyCoGuestAdapter(Context context, AnchorManager manager,
                                     LiveCoreView liveStream) {
        mContext = context;
        mLiveManage = manager;
        mLiveStream = liveStream;
        initData();
    }

    @NonNull
    @Override
    public ApplyLinkMicViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(mContext).inflate(
                R.layout.livekit_layout_anchor_link_mic_panel_item_request, parent, false);
        return new ApplyLinkMicViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ApplyLinkMicViewHolder holder, int position) {
        if (TextUtils.isEmpty(mData.get(position).userName)) {
            holder.textName.setText(mData.get(position).userId);
        } else {
            holder.textName.setText(mData.get(position).userName);
        }

        if (TextUtils.isEmpty(mData.get(position).avatarUrl)) {
            holder.imageHead.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, holder.imageHead, mData.get(position).avatarUrl, R.drawable.livekit_ic_avatar);
        }
        holder.textReject.setTag(mData.get(position));
        holder.textReject.setEnabled(true);
        holder.textReject.setOnClickListener((view) -> {
            view.setEnabled(false);
            final TUIRoomDefine.UserInfo userInfo = (TUIRoomDefine.UserInfo) view.getTag();
            mLiveStream.respondIntraRoomConnection(userInfo.userId, false, new TUIRoomDefine.ActionCallback() {
                @Override
                public void onSuccess() {
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    ErrorLocalized.onError(error);
                }
            });
        });

        holder.textAccept.setTag(mData.get(position));
        holder.textAccept.setEnabled(true);
        holder.textAccept.setOnClickListener((view) -> {
            view.setEnabled(false);
            final TUIRoomDefine.UserInfo userInfo = (TUIRoomDefine.UserInfo) view.getTag();
            mLiveStream.respondIntraRoomConnection(userInfo.userId, true,
                    new TUIRoomDefine.ActionCallback() {
                        @Override
                        public void onSuccess() {
                        }

                        @Override
                        public void onError(TUICommonDefine.Error error, String message) {
                            if (error == ALL_SEAT_OCCUPIED) {
                                view.setEnabled(true);
                            }
                            ErrorLocalized.onError(error);
                            LOGGER.error("AnchorApplyCoGuestAdapter" + " respondIntraRoomConnection " +
                                    "failed:error:" + error +
                                    ",errorCode:" + error.getValue() + "message:" + message);
                        }
                    });
        });
    }

    private void initData() {
        mData.clear();
        mData.addAll(mLiveManage.getCoreState().coGuestState.applicantList.getValue());
    }

    @SuppressLint("NotifyDataSetChanged")
    public void updateData() {
        initData();
        notifyDataSetChanged();
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    public static class ApplyLinkMicViewHolder extends RecyclerView.ViewHolder {
        public ImageFilterView imageHead;
        public TextView        textName;
        public TextView        textLevel;
        public TextView        textAccept;
        public TextView        textReject;

        public ApplyLinkMicViewHolder(View itemView) {
            super(itemView);
            imageHead = itemView.findViewById(R.id.iv_head);
            textName = itemView.findViewById(R.id.tv_name);
            textLevel = itemView.findViewById(R.id.tv_level);
            textAccept = itemView.findViewById(R.id.tv_accept);
            textReject = itemView.findViewById(R.id.tv_reject);
        }
    }
}

