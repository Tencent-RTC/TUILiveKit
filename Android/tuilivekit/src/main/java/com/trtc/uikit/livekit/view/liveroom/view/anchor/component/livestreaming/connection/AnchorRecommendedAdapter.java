package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming.connection;

import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.constraintlayout.utils.widget.ImageFilterView;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.ConnectionState;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

public class AnchorRecommendedAdapter extends RecyclerView.Adapter<AnchorRecommendedAdapter.RecommendViewHolder> {

    private final LiveController mLiveController;
    private final Context        mContext;

    private final CopyOnWriteArrayList<ConnectionState.ConnectionUser> mData = new CopyOnWriteArrayList<>();

    public AnchorRecommendedAdapter(Context context, LiveController liveController,
                                    List<ConnectionState.ConnectionUser> recommendList) {
        mContext = context;
        mLiveController = liveController;
        initData(recommendList);
    }

    @NonNull
    @Override
    public RecommendViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(mContext).inflate(
                R.layout.livekit_layout_anchor_connection_recommendation_list, parent, false);
        return new RecommendViewHolder(view);
    }

    public void updateData(List<ConnectionState.ConnectionUser> recommendList) {
        mData.clear();
        for (ConnectionState.ConnectionUser recommendUser : recommendList) {
            if (!TextUtils.equals(recommendUser.userId, mLiveController.getUserState().selfInfo.userId)) {
                mData.add(recommendUser);
            }
        }
    }

    @Override
    public void onBindViewHolder(@NonNull RecommendViewHolder holder, int position) {
        ConnectionState.ConnectionUser recommendUser = mData.get(position);

        setUserName(holder, recommendUser);
        setAvatar(holder, recommendUser);
        setConnectionStatus(holder, recommendUser);
        setConnectionClickListener(holder, recommendUser);
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    private void setUserName(RecommendViewHolder holder, ConnectionState.ConnectionUser recommendUser) {
        holder.textName.setText(TextUtils.isEmpty(recommendUser.userName)
                ? recommendUser.roomId : recommendUser.userName);
    }

    private void setAvatar(RecommendViewHolder holder, ConnectionState.ConnectionUser recommendUser) {
        if (TextUtils.isEmpty(recommendUser.avatarUrl)) {
            holder.imageHead.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, holder.imageHead, recommendUser.avatarUrl, R.drawable.livekit_ic_avatar);
        }
    }

    private void setConnectionStatus(RecommendViewHolder holder, ConnectionState.ConnectionUser recommendUser) {
        if (recommendUser.connectionStatus == ConnectionState.ConnectionStatus.INVITING) {
            holder.textConnect.setText(R.string.livekit_connect_inviting);
            holder.textConnect.setAlpha(0.5f);
        } else {
            holder.textConnect.setText(R.string.livekit_start_connection);
            holder.textConnect.setAlpha(1f);
        }
    }

    private void setConnectionClickListener(RecommendViewHolder holder, ConnectionState.ConnectionUser recommendUser) {
        holder.textConnect.setOnClickListener(view -> {
            if (recommendUser.connectionStatus == ConnectionState.ConnectionStatus.UNKNOWN) {
                List<String> roomIdList = Collections.singletonList(recommendUser.roomId);
                int timeoutSeconds = 10;
                mLiveController.getConnectionController().requestConnection(roomIdList, timeoutSeconds, null);
            }
        });
    }

    private void initData(List<ConnectionState.ConnectionUser> recommendList) {
        mData.clear();
        for (ConnectionState.ConnectionUser recommendUser : recommendList) {
            if (!TextUtils.equals(recommendUser.userId, mLiveController.getUserState().selfInfo.userId)) {
                mData.add(recommendUser);
            }
        }
    }

    public static class RecommendViewHolder extends RecyclerView.ViewHolder {
        public ImageFilterView imageHead;
        public TextView        textName;
        public TextView        textLevel;
        public TextView        textConnect;

        public RecommendViewHolder(View itemView) {
            super(itemView);
            imageHead = itemView.findViewById(R.id.iv_head);
            textName = itemView.findViewById(R.id.tv_name);
            textLevel = itemView.findViewById(R.id.tv_level);
            textConnect = itemView.findViewById(R.id.tv_connect);
        }
    }
}

