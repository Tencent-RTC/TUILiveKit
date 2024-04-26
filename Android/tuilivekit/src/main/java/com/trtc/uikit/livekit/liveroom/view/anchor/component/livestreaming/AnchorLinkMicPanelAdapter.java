package com.trtc.uikit.livekit.liveroom.view.anchor.component.livestreaming;

import static com.trtc.uikit.livekit.liveroom.view.anchor.component.livestreaming.AnchorLinkMicPanelAdapter.ViewType.MIC_DOWN_ITEM;
import static com.trtc.uikit.livekit.liveroom.view.anchor.component.livestreaming.AnchorLinkMicPanelAdapter.ViewType.MIC_DOWN_TITLE;
import static com.trtc.uikit.livekit.liveroom.view.anchor.component.livestreaming.AnchorLinkMicPanelAdapter.ViewType.MIC_UP_ITEM;
import static com.trtc.uikit.livekit.liveroom.view.anchor.component.livestreaming.AnchorLinkMicPanelAdapter.ViewType.MIC_UP_TITLE;

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

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.core.LiveKitStore;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.data.UserInfo;

import java.util.concurrent.CopyOnWriteArrayList;

public class AnchorLinkMicPanelAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {

    private static final String TYPE_MIC_UP   = "type_mic_up";
    private static final String TYPE_MIC_DOWN = "type_mic_down";

    private final Context                        mContext;
    private final LiveRoomInfo                   mLiveRoomInfo;
    private final RoomEngineService              mRoomEngineService;
    private final CopyOnWriteArrayList<UserInfo> mData = new CopyOnWriteArrayList<>();

    public AnchorLinkMicPanelAdapter(Context context, LiveRoomInfo roomInfo, RoomEngineService service) {
        mContext = context;
        mLiveRoomInfo = roomInfo;
        mRoomEngineService = service;
        initData();
    }

    @NonNull
    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view;
        if (viewType == MIC_UP_TITLE.ordinal()) {
            view = LayoutInflater.from(mContext).inflate(
                    R.layout.livekit_layout_anchor_link_mic_panel_item_title, parent, false);
            return new TitleItemViewHolder(view);
        } else if (viewType == MIC_UP_ITEM.ordinal()) {
            view = LayoutInflater.from(mContext).inflate(
                    R.layout.livekit_layout_anchor_link_mic_panel_item_up, parent, false);
            return new MicUpItemViewHolder(view);
        } else if (viewType == MIC_DOWN_TITLE.ordinal()) {
            view = LayoutInflater.from(mContext).inflate(
                    R.layout.livekit_layout_anchor_link_mic_panel_item_title, parent, false);
            return new TitleItemViewHolder(view);
        } else {
            view = LayoutInflater.from(mContext).inflate(
                    R.layout.livekit_layout_anchor_link_mic_panel_item_request, parent, false);
            return new MicDownItemViewHolder(view);
        }
    }

    @Override
    public void onBindViewHolder(@NonNull RecyclerView.ViewHolder holder, int position) {
        int viewType = getItemViewType(position);
        if (viewType == MIC_UP_TITLE.ordinal()) {
            bindMicUpTitleViewHolder((TitleItemViewHolder) holder);
        } else if (viewType == MIC_UP_ITEM.ordinal()) {
            bindMicUpItemViewHolder((MicUpItemViewHolder) holder, position);
        } else if (viewType == MIC_DOWN_TITLE.ordinal()) {
            bindMicDownTitleViewHolder((TitleItemViewHolder) holder);
        } else {
            bindMicDownItemViewHolder((MicDownItemViewHolder) holder, position);
        }
    }

    private void initData() {
        mData.clear();
        if (!mLiveRoomInfo.linkingAudienceList.get().isEmpty()) {
            UserInfo info = new UserInfo();
            info.name.set(TYPE_MIC_UP);
            mData.add(info);
        }
        mData.addAll(mLiveRoomInfo.linkingAudienceList.get());
        if (!LiveKitStore.sharedInstance().applyLinkAudienceList.get().isEmpty()) {
            UserInfo info = new UserInfo();
            info.name.set(TYPE_MIC_DOWN);
            mData.add(info);
        }
        mData.addAll(LiveKitStore.sharedInstance().applyLinkAudienceList.get());
    }

    @SuppressLint("NotifyDataSetChanged")
    public void updateData() {
        initData();
        notifyDataSetChanged();
    }

    private void bindMicDownItemViewHolder(MicDownItemViewHolder holder, int position) {
        if (TextUtils.isEmpty(mData.get(position).name.get())) {
            holder.textName.setText(mData.get(position).userId);
        } else {
            holder.textName.setText(mData.get(position).name.get());
        }

        if (TextUtils.isEmpty(mData.get(position).avatarUrl.get())) {
            holder.imageHead.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, holder.imageHead, mData.get(position).avatarUrl.get(),
                    R.drawable.livekit_ic_avatar);
        }
        holder.textReject.setTag(mData.get(position));
        holder.textReject.setOnClickListener((view) -> {
            final UserInfo userInfo = (UserInfo) view.getTag();
            mRoomEngineService.responseRemoteRequestUser(userInfo, false, null);
        });

        holder.textAccept.setTag(mData.get(position));
        holder.textAccept.setOnClickListener((view) -> {
            final UserInfo userInfo = (UserInfo) view.getTag();
            mRoomEngineService.responseRemoteRequestUser(userInfo, true, null);
        });
    }

    private void bindMicUpItemViewHolder(MicUpItemViewHolder holder, int position) {
        if (TextUtils.isEmpty(mData.get(position).name.get())) {
            holder.textName.setText(mData.get(position).userId);
        } else {
            holder.textName.setText(mData.get(position).name.get());
        }

        if (TextUtils.isEmpty(mData.get(position).avatarUrl.get())) {
            holder.imageHead.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, holder.imageHead, mData.get(position).avatarUrl.get(),
                    R.drawable.livekit_ic_avatar);
        }
        holder.textHangUp.setTag(mData.get(position));
        holder.textHangUp.setOnClickListener((view) -> {
            final UserInfo userInfo = (UserInfo) view.getTag();
            mRoomEngineService.kickUserOffSeatByAdmin(0, userInfo, null);
        });
    }

    private void bindMicDownTitleViewHolder(TitleItemViewHolder holder) {
        if (!LiveKitStore.sharedInstance().applyLinkAudienceList.get().isEmpty()) {
            holder.viewSeparation.setVisibility(View.VISIBLE);
        } else {
            holder.viewSeparation.setVisibility(View.GONE);
        }
        holder.tvTitle.setText(mContext.getString(R.string.livekit_link_mic_down_title,
                LiveKitStore.sharedInstance().applyLinkAudienceList.get().size()));
    }

    private void bindMicUpTitleViewHolder(TitleItemViewHolder holder) {
        holder.viewSeparation.setVisibility(View.GONE);
        holder.tvTitle.setText(mContext.getString(R.string.livekit_link_mic_up_title,
                mLiveRoomInfo.linkingAudienceList.get().size(), mLiveRoomInfo.maxSeatCount));
    }

    @Override
    public int getItemViewType(int position) {
        if (mLiveRoomInfo.linkingAudienceList.get().contains(mData.get(position))) {
            return MIC_UP_ITEM.ordinal();
        } else if (LiveKitStore.sharedInstance().applyLinkAudienceList.get().contains(mData.get(position))) {
            return MIC_DOWN_ITEM.ordinal();
        } else if (TYPE_MIC_UP.equals(mData.get(0).name.get())) {
            return MIC_UP_TITLE.ordinal();
        } else {
            return MIC_DOWN_TITLE.ordinal();
        }
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    public static class MicUpItemViewHolder extends RecyclerView.ViewHolder {
        public ImageFilterView imageHead;
        public TextView        textName;
        public TextView        textLevel;
        public TextView        textHangUp;

        public MicUpItemViewHolder(View itemView) {
            super(itemView);
            imageHead = itemView.findViewById(R.id.iv_head);
            textName = itemView.findViewById(R.id.tv_name);
            textLevel = itemView.findViewById(R.id.tv_level);
            textHangUp = itemView.findViewById(R.id.tv_hang_up);
        }
    }


    public static class MicDownItemViewHolder extends RecyclerView.ViewHolder {
        public ImageFilterView imageHead;
        public TextView        textName;
        public TextView        textLevel;
        public TextView        textAccept;
        public TextView        textReject;

        public MicDownItemViewHolder(View itemView) {
            super(itemView);
            imageHead = itemView.findViewById(R.id.iv_head);
            textName = itemView.findViewById(R.id.tv_name);
            textLevel = itemView.findViewById(R.id.tv_level);
            textAccept = itemView.findViewById(R.id.tv_accept);
            textReject = itemView.findViewById(R.id.tv_reject);
        }
    }


    public static class TitleItemViewHolder extends RecyclerView.ViewHolder {
        public TextView tvTitle;
        public View     viewSeparation;

        public TitleItemViewHolder(View itemView) {
            super(itemView);
            viewSeparation = itemView.findViewById(R.id.view_separation);
            tvTitle = itemView.findViewById(R.id.tv_title);
        }
    }

    enum ViewType {
        MIC_UP_TITLE, MIC_UP_ITEM, MIC_DOWN_TITLE, MIC_DOWN_ITEM
    }
}
