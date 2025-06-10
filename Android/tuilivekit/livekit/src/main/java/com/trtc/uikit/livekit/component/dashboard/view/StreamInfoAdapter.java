package com.trtc.uikit.livekit.component.dashboard.view;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.trtc.TRTCStatistics;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.dashboard.store.StreamDashboardUserState;

import java.util.ArrayList;
import java.util.List;

public class StreamInfoAdapter extends RecyclerView.Adapter<StreamInfoAdapter.ViewHolder> {
    private final Context                        mContext;
    private final List<StreamDashboardUserState> mDataList;

    public StreamInfoAdapter(Context context, List<StreamDashboardUserState> dataList) {
        mContext = context;
        mDataList = dataList;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.livekit_adapter_item_stream_info,
                parent, false);
        return new ViewHolder(view);
    }

    @SuppressLint("SetTextI18n")
    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        StreamDashboardUserState streamDashboardUserState = mDataList.get(position);
        String title;
        if (streamDashboardUserState.isLocal) {
            title = mContext.getString(R.string.common_dashboard_local_user);
            holder.mUserInfoLayout.setVisibility(getItemCount() == 1 ? View.GONE : View.VISIBLE);
        } else {
            title = mContext.getString(R.string.common_dashboard_remote_user) + " : " + streamDashboardUserState.userId;
            holder.mUserInfoLayout.setVisibility(View.VISIBLE);
        }
        holder.mTextUserId.setText(title);
        holder.mTextVideoResolution.setText(streamDashboardUserState.videoResolution);
        holder.mTextVideoBitrate.setText(streamDashboardUserState.videoBitrate + " kbps");
        holder.mTextVideoFps.setText(streamDashboardUserState.videoFrameRate + " FPS");
        holder.mTextAudioSampleRate.setText(streamDashboardUserState.audioSampleRate + " HZ");
        holder.mTextAudioBitrate.setText(streamDashboardUserState.audioBitrate + " kbps");
    }

    @SuppressLint("NotifyDataSetChanged")
    public void updateLocalVideoStatus(ArrayList<TRTCStatistics.TRTCLocalStatistics> localArray) {
        if (localArray == null) {
            return;
        }
        for (TRTCStatistics.TRTCLocalStatistics localStatistics : localArray) {
            mDataList.clear();
            mDataList.add(new StreamDashboardUserState("", true,
                    localStatistics.width + "*" + localStatistics.height, localStatistics.frameRate,
                    localStatistics.videoBitrate, localStatistics.audioSampleRate, localStatistics.audioBitrate));
        }
        notifyDataSetChanged();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void updateRemoteVideoStatus(ArrayList<TRTCStatistics.TRTCRemoteStatistics> remoteArray) {
        if (remoteArray == null) {
            return;
        }
        if (!mDataList.isEmpty()) {
            if (mDataList.get(0).isLocal) {
                mDataList.subList(1, mDataList.size()).clear();
            } else {
                mDataList.clear();
            }
        }
        for (TRTCStatistics.TRTCRemoteStatistics remoteStatistics : remoteArray) {
            mDataList.add(new StreamDashboardUserState(remoteStatistics.userId, false,
                    remoteStatistics.width + "*" + remoteStatistics.height,
                    remoteStatistics.frameRate, remoteStatistics.videoBitrate,
                    remoteStatistics.audioSampleRate, remoteStatistics.audioBitrate));
        }
        notifyDataSetChanged();
    }

    @Override
    public int getItemCount() {
        return mDataList.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        private final LinearLayout mUserInfoLayout;
        private final TextView     mTextUserId;
        private final TextView     mTextVideoResolution;
        private final TextView     mTextVideoBitrate;
        private final TextView     mTextVideoFps;
        private final TextView     mTextAudioSampleRate;
        private final TextView     mTextAudioBitrate;

        public ViewHolder(@NonNull View itemView) {
            super(itemView);
            mUserInfoLayout = itemView.findViewById(R.id.ll_user_info);
            mTextUserId = itemView.findViewById(R.id.tv_user_id);
            mTextVideoResolution = itemView.findViewById(R.id.tv_video_resolution);
            mTextVideoBitrate = itemView.findViewById(R.id.tv_video_bitrate);
            mTextVideoFps = itemView.findViewById(R.id.tv_video_fps);
            mTextAudioSampleRate = itemView.findViewById(R.id.tv_audio_sample_rate);
            mTextAudioBitrate = itemView.findViewById(R.id.tv_audio_bitrate);
        }
    }
}
