package com.trtc.uikit.livekit.features.anchorboardcast.view.cohost.panel;

import static com.trtc.uikit.livekit.features.anchorboardcast.state.CoHostState.ConnectionStatus.INVITING;
import static com.trtc.uikit.livekit.features.anchorboardcast.state.CoHostState.ConnectionStatus.UNKNOWN;

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
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.error.ConnectionErrorHandler;
import com.trtc.uikit.livekit.features.anchorboardcast.state.CoHostState;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

public class AnchorRecommendedAdapter extends RecyclerView.Adapter<AnchorRecommendedAdapter.RecommendViewHolder> {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("AnchorRecommendedAdapter");

    private final AnchorManager                                    mAnchorManager;
    private final LiveCoreView                                     mLiveStream;
    private final Context                                          mContext;
    private final CopyOnWriteArrayList<CoHostState.ConnectionUser> mData = new CopyOnWriteArrayList<>();

    public AnchorRecommendedAdapter(Context context, AnchorManager liveStreamManager,
                                    LiveCoreView liveStream) {
        mContext = context;
        mAnchorManager = liveStreamManager;
        mLiveStream = liveStream;
        initData(mAnchorManager.getCoHostState().recommendUsers.getValue());
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
            if (!TextUtils.equals(recommendUser.userId,
                    mAnchorManager.getCoreState().userState.selfInfo.getValue().userId)) {
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
                ? recommendUser.userId : recommendUser.userName);
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
            holder.textConnect.setText(R.string.common_connect_inviting);
            holder.textConnect.setAlpha(0.5f);
        } else {
            holder.textConnect.setText(R.string.common_voiceroom_invite);
            holder.textConnect.setAlpha(1f);
        }
    }

    private void setConnectionClickListener(RecommendViewHolder holder, CoHostState.ConnectionUser recommendUser) {
        holder.textConnect.setOnClickListener(view -> {
            if (recommendUser.connectionStatus == CoHostState.ConnectionStatus.UNKNOWN) {
                recommendUser.connectionStatus = INVITING;
                mAnchorManager.getCoHostState().recommendUsers.setValue(
                        mAnchorManager.getCoHostState().recommendUsers.getValue());
                mLiveStream.requestCrossRoomConnection(recommendUser.roomId, 10,
                        new TUILiveConnectionManager.ConnectionRequestCallback() {
                            @Override
                            public void onSuccess(Map<String, TUILiveConnectionManager.ConnectionCode> map) {
                                if (map != null) {
                                    TUILiveConnectionManager.ConnectionCode code = map.get(recommendUser.roomId);
                                    if (code != TUILiveConnectionManager.ConnectionCode.SUCCESS) {
                                        recommendUser.connectionStatus = UNKNOWN;
                                        mAnchorManager.getCoHostState().recommendUsers.setValue(
                                                mAnchorManager.getCoHostState().recommendUsers.getValue());
                                        ConnectionErrorHandler.onError(code);
                                    }
                                }
                            }

                            @Override
                            public void onError(TUICommonDefine.Error error, String message) {
                                recommendUser.connectionStatus = UNKNOWN;
                                mAnchorManager.getCoHostState().recommendUsers.setValue(
                                        mAnchorManager.getCoHostState().recommendUsers.getValue());
                                ErrorLocalized.onError(error);
                                LOGGER.error("AnchorRecommendedAdapter" + " requestCrossRoomConnection " +
                                        "failed:error:" + error + "," + "errorCode:" + error.getValue() + "message:" + message);
                            }
                        });
            }
        });
    }

    private void initData(List<CoHostState.ConnectionUser> recommendList) {
        mData.clear();
        for (CoHostState.ConnectionUser recommendUser : recommendList) {
            if (!TextUtils.equals(recommendUser.userId,
                    mAnchorManager.getCoreState().userState.selfInfo.getValue().userId)) {
                mData.add(recommendUser);
            }
        }
    }

    public static class RecommendViewHolder extends RecyclerView.ViewHolder {
        public ImageFilterView imageHead;
        public TextView        textName;
        public TextView        textConnect;

        public RecommendViewHolder(View itemView) {
            super(itemView);
            imageHead = itemView.findViewById(R.id.iv_head);
            textName = itemView.findViewById(R.id.tv_name);
            textConnect = itemView.findViewById(R.id.tv_connect);
        }
    }
}

