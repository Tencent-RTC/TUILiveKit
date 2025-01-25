package com.trtc.uikit.livekit.livestream.view.anchor.pushing.cohost;

import static com.trtc.uikit.livekit.livestream.state.CoHostState.ConnectionStatus.INVITING;

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
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.manager.error.ConnectionErrorHandler;
import com.trtc.uikit.livekit.livestream.manager.error.ErrorHandler;
import com.trtc.uikit.livekit.livestream.state.CoHostState;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

public class AnchorRecommendedAdapter extends RecyclerView.Adapter<AnchorRecommendedAdapter.RecommendViewHolder> {

    private final LiveStreamManager                                mLiveStreamManager;
    private final LiveCoreView                                     mLiveStream;
    private final Context                                          mContext;
    private final CopyOnWriteArrayList<CoHostState.ConnectionUser> mData = new CopyOnWriteArrayList<>();

    public AnchorRecommendedAdapter(Context context, LiveStreamManager liveStreamManager,
                                    LiveCoreView liveStream) {
        mContext = context;
        mLiveStreamManager = liveStreamManager;
        mLiveStream = liveStream;
        initData(mLiveStreamManager.getCoHostState().recommendUsers.get());
    }

    @NonNull
    @Override
    public RecommendViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(mContext).inflate(
                R.layout.livekit_layout_anchor_connection_recommendation_list, parent, false);
        return new RecommendViewHolder(view);
    }

    public void updateData(List<CoHostState.ConnectionUser> recommendList) {
        mData.clear();
        for (CoHostState.ConnectionUser recommendUser : recommendList) {
            if (!TextUtils.equals(recommendUser.userId, mLiveStreamManager.getUserState().selfInfo.userId)) {
                mData.add(recommendUser);
            }
        }
    }

    @Override
    public void onBindViewHolder(@NonNull RecommendViewHolder holder, int position) {
        CoHostState.ConnectionUser recommendUser = mData.get(position);

        setUserName(holder, recommendUser);
        setAvatar(holder, recommendUser);
        setConnectionStatus(holder, recommendUser);
        setConnectionClickListener(holder, recommendUser);
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    private void setUserName(RecommendViewHolder holder, CoHostState.ConnectionUser recommendUser) {
        holder.textName.setText(TextUtils.isEmpty(recommendUser.userName)
                ? recommendUser.roomId : recommendUser.userName);
    }

    private void setAvatar(RecommendViewHolder holder, CoHostState.ConnectionUser recommendUser) {
        if (TextUtils.isEmpty(recommendUser.avatarUrl)) {
            holder.imageHead.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, holder.imageHead, recommendUser.avatarUrl, R.drawable.livekit_ic_avatar);
        }
    }

    private void setConnectionStatus(RecommendViewHolder holder, CoHostState.ConnectionUser recommendUser) {
        if (recommendUser.connectionStatus == CoHostState.ConnectionStatus.INVITING) {
            holder.textConnect.setText(R.string.livekit_connect_inviting);
            holder.textConnect.setAlpha(0.5f);
        } else {
            holder.textConnect.setText(R.string.livekit_start_connection);
            holder.textConnect.setAlpha(1f);
        }
    }

    private void setConnectionClickListener(RecommendViewHolder holder, CoHostState.ConnectionUser recommendUser) {
        holder.textConnect.setOnClickListener(view -> {
            if (recommendUser.connectionStatus == CoHostState.ConnectionStatus.UNKNOWN) {
                mLiveStream.requestCrossRoomConnection(recommendUser.roomId, 10,
                        new TUILiveConnectionManager.ConnectionRequestCallback() {
                    @Override
                    public void onSuccess(Map<String, TUILiveConnectionManager.ConnectionCode> map) {
                        if (map != null) {
                            TUILiveConnectionManager.ConnectionCode code = map.get(recommendUser.roomId);
                            if (code == TUILiveConnectionManager.ConnectionCode.SUCCESS) {
                                for (CoHostState.ConnectionUser item :
                                        mLiveStreamManager.getCoHostState().recommendUsers.get()) {
                                    if (TextUtils.equals(item.roomId, recommendUser.roomId)) {
                                        item.connectionStatus = INVITING;
                                        mLiveStreamManager.getCoHostManager().addSendConnectionRequest(item);
                                        mLiveStreamManager.getCoHostState().recommendUsers.notifyDataChanged();
                                    }
                                }
                            } else {
                                ConnectionErrorHandler.onError(code);
                            }
                        }
                    }

                    @Override
                    public void onError(TUICommonDefine.Error error, String message) {
                        ErrorHandler.onError(error);
                    }
                });
            }
        });
    }

    private void initData(List<CoHostState.ConnectionUser> recommendList) {
        mData.clear();
        for (CoHostState.ConnectionUser recommendUser : recommendList) {
            if (!TextUtils.equals(recommendUser.userId, mLiveStreamManager.getUserState().selfInfo.userId)) {
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

