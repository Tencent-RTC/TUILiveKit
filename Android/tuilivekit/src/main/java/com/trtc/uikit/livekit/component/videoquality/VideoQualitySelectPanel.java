package com.trtc.uikit.livekit.component.videoquality;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;

import java.util.List;

public class VideoQualitySelectPanel extends PopupDialog {

    private final List<TUIRoomDefine.VideoQuality> mVideoQualityLists;
    private       RecyclerView                     mRecyclerView;
    private       TextView                         mCancelButton;
    private       OnVideoQualitySelectedListener   mListener;

    private final TUIRoomObserver mRoomObserver = new TUIRoomObserver() {
        @Override
        public void onRoomDismissed(String roomId, TUIRoomDefine.RoomDismissedReason reason) {
            dismiss();
        }
    };

    public interface OnVideoQualitySelectedListener {
        void onVideoQualitySelected(TUIRoomDefine.VideoQuality videoQuality);
    }

    public VideoQualitySelectPanel(Context context, List<TUIRoomDefine.VideoQuality> videoQualityList) {
        super(context);
        mVideoQualityLists = videoQualityList;
        initView();
    }

    public void setOnVideoQualitySelectedListener(OnVideoQualitySelectedListener listener) {
        mListener = listener;
    }

    @Override
    protected void onStart() {
        super.onStart();
        TUIRoomEngine.sharedInstance().addObserver(mRoomObserver);
    }

    @Override
    protected void onStop() {
        TUIRoomEngine.sharedInstance().removeObserver(mRoomObserver);
        super.onStop();
    }

    private void initView() {
        View view = View.inflate(getContext(), R.layout.livekit_layout_video_quality_select_panel, null);
        mRecyclerView = view.findViewById(R.id.rv_resolution_options);
        mCancelButton = view.findViewById(R.id.tv_cancel);

        setupRecyclerView();
        setupCancelButton();

        setView(view);
    }

    private void setupRecyclerView() {
        mRecyclerView.setLayoutManager(new LinearLayoutManager(getContext()));
        ResolutionAdapter adapter = new ResolutionAdapter(mVideoQualityLists);
        mRecyclerView.setAdapter(adapter);
    }

    private void setupCancelButton() {
        mCancelButton.setOnClickListener(v -> {
            dismiss();
        });
    }

    private class ResolutionAdapter extends RecyclerView.Adapter<ResolutionAdapter.ViewHolder> {
        private final List<TUIRoomDefine.VideoQuality> mData;

        public ResolutionAdapter(List<TUIRoomDefine.VideoQuality> data) {
            mData = data;
        }

        @NonNull
        @Override
        public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            View view = LayoutInflater.from(parent.getContext())
                    .inflate(R.layout.livekit_recycler_item_video_quality, parent, false);
            return new ViewHolder(view);
        }

        @Override
        public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
            TUIRoomDefine.VideoQuality videoQuality = mData.get(position);
            holder.textView.setText(videoQualityToString(videoQuality));

            if (position == mData.size() - 1) {
                holder.divider.setVisibility(View.GONE);
            } else {
                holder.divider.setVisibility(View.VISIBLE);
            }

            holder.textView.setOnClickListener(v -> {
                if (mListener != null) {
                    mListener.onVideoQualitySelected(videoQuality);
                }
                dismiss();
            });
        }

        @Override
        public int getItemCount() {
            return mData != null ? mData.size() : 0;
        }

        private String videoQualityToString(TUIRoomDefine.VideoQuality quality) {
            switch (quality) {
                case Q_1080P:
                    return "1080P";
                case Q_720P:
                    return "720P";
                case Q_540P:
                    return "540P";
                case Q_360P:
                    return "360P";
                default:
                    return "original";
            }
        }

        class ViewHolder extends RecyclerView.ViewHolder {
            TextView textView;
            View     divider;

            ViewHolder(View itemView) {
                super(itemView);
                textView = itemView.findViewById(R.id.tv_resolution_text);
                divider = itemView.findViewById(R.id.divider);
            }
        }
    }
}
