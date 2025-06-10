package com.trtc.uikit.livekit.features.anchorboardcast.view.cohost.managerpanel;

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
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;
import com.trtc.uikit.livekit.livestream.manager.error.ConnectionErrorHandler;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

public class RecommendAdapter extends RecyclerView.Adapter<RecommendAdapter.RecommendViewHolder> {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("AnchorRecommendedAdapter");

    private final AnchorManager mManager;
    private final LiveCoreView  mCoreView;
    private final Context                              mContext;
    private final CopyOnWriteArrayList<ConnectionUser> mData = new CopyOnWriteArrayList<>();

    public RecommendAdapter(Context context, AnchorManager manager) {
        mContext = context;
        mManager = manager;
        mCoreView = mManager.getCoreView();
        initData(mManager.getState().recommendUsers.getValue());
    }

    @NonNull
    @Override
    public RecommendViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(mContext).inflate(
                R.layout.livekit_layout_anchor_connection_recommendation_list, parent, false);
        return new RecommendViewHolder(view);
    }

    public void updateData(List<ConnectionUser> recommendList) {
        mData.clear();
        for (ConnectionUser recommendUser : recommendList) {
            if (!TextUtils.equals(recommendUser.userId, mManager.getState().loginUserInfo.userId)) {
                mData.add(recommendUser);
            }
        }
    }

    @Override
    public void onBindViewHolder(@NonNull RecommendViewHolder holder, int position) {
        ConnectionUser recommendUser = mData.get(position);

        setUserName(holder, recommendUser);
        setAvatar(holder, recommendUser);
        setConnectionStatus(holder, recommendUser);
        setConnectionClickListener(holder, recommendUser);
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    private void setUserName(RecommendViewHolder holder, ConnectionUser recommendUser) {
        holder.textName.setText(TextUtils.isEmpty(recommendUser.userName)
                ? recommendUser.roomId : recommendUser.userName);
    }

    private void setAvatar(RecommendViewHolder holder, ConnectionUser recommendUser) {
        if (TextUtils.isEmpty(recommendUser.avatarUrl)) {
            holder.imageHead.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, holder.imageHead, recommendUser.avatarUrl, R.drawable.livekit_ic_avatar);
        }
    }

    private void setConnectionStatus(RecommendViewHolder holder,
                                     ConnectionUser recommendUser) {
        if (mManager.isInvitingConnectionUser(recommendUser)) {
            holder.textConnect.setText(R.string.common_connect_inviting);
            holder.textConnect.setAlpha(0.5f);
        } else {
            holder.textConnect.setText(R.string.common_voiceroom_invite);
            holder.textConnect.setAlpha(1f);
        }
    }

    private void setConnectionClickListener(RecommendViewHolder holder,
                                            ConnectionUser recommendUser) {
        holder.textConnect.setOnClickListener(view -> {
            if (mManager.isInvitingConnectionUser(recommendUser) || mManager.isConnectedConnectionUser(recommendUser)) {
                return;
            }
            mCoreView.requestCrossRoomConnection(recommendUser.roomId, 10,
                    new TUILiveConnectionManager.ConnectionRequestCallback() {
                        @Override
                        public void onSuccess(Map<String, TUILiveConnectionManager.ConnectionCode> map) {
                            if (map != null) {
                                TUILiveConnectionManager.ConnectionCode code = map.get(recommendUser.roomId);
                                if (code == TUILiveConnectionManager.ConnectionCode.SUCCESS) {
                                    notifyDataSetChanged();
                                } else {
                                    ConnectionErrorHandler.onError(code);
                                }
                            }
                        }

                        @Override
                        public void onError(TUICommonDefine.Error error, String message) {
                            ErrorLocalized.onError(error);
                            LOGGER.error("AnchorRecommendedAdapter" + " requestCrossRoomConnection " +
                                    "failed:error:" + error + "," + "errorCode:" + error.getValue() + "message:" + message);
                        }
                    });
        });
    }

    private void initData(List<ConnectionUser> recommendList) {
        mData.clear();
        for (ConnectionUser recommendUser : recommendList) {
            if (!TextUtils.equals(recommendUser.userId, mManager.getState().loginUserInfo.userId)) {
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

