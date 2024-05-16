package com.trtc.uikit.livekit.common.uicomponent.audioeffect;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.ui.ConfirmWithCheckboxDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveController;
import com.trtc.uikit.livekit.common.core.controller.MediaController;
import com.trtc.uikit.livekit.common.core.store.state.operation.MediaState;
import com.trtc.uikit.livekit.liveroom.data.MusicInfo;

public class MusicListAdapter extends RecyclerView.Adapter<MusicListAdapter.ViewHolder> {

    private final Context         mContext;
    private final MediaController mMediaController;
    private final MediaState      mMediaState;
    private       int             mSelectedPosition;

    public MusicListAdapter(Context context, LiveController liveController) {
        mContext = context;
        mMediaController = liveController.getMediaController();
        mMediaState = liveController.getMediaState();
        initData();
    }

    private void initData() {
        if (mMediaState.musicList.isEmpty()) {
            mMediaState.musicList.add(new MusicInfo(1, mContext.getString(R.string.livekit_music_cheerful),
                    "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/PositiveHappyAdvertising.mp3"));
            mMediaState.musicList.add(new MusicInfo(2, mContext.getString(R.string.livekit_music_melancholy),
                    "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/SadCinematicPiano.mp3"));
            mMediaState.musicList.add(new MusicInfo(3, mContext.getString(R.string.livekit_music_wonder_world),
                    "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/WonderWorld.mp3"));
        }
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.livekit_recycle_item_music, parent,
                false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(ViewHolder holder, int position) {
        final MusicInfo musicInfo = mMediaState.musicList.get(position);
        holder.textMusicName.setText(musicInfo.name);
        if (musicInfo.isPlaying.get()) {
            holder.imageStartStop.setImageResource(R.drawable.livekit_music_pause);
            mSelectedPosition = mMediaState.musicList.indexOf(musicInfo);
        } else {
            holder.imageStartStop.setImageResource(R.drawable.livekit_music_start);
        }
        holder.imageDelete.setOnClickListener(view -> {
            ConfirmWithCheckboxDialog dialog = new ConfirmWithCheckboxDialog(mContext);
            dialog.setTitle(mContext.getString(R.string.livekit_tips_title));
            dialog.setContent(mContext.getString(R.string.livekit_musuic_delete_tips, musicInfo.name));
            dialog.setNegativeText(mContext.getString(R.string.livekit_cancel), negativeView -> dialog.dismiss());
            dialog.setPositiveText(mContext.getString(R.string.livekit_confirm), positiveView -> {
                int index = holder.getBindingAdapterPosition();
                if (index != RecyclerView.NO_POSITION) {
                    mMediaController.deleteMusic(musicInfo);
                    notifyItemRemoved(index);
                    dialog.dismiss();
                }
            });
            dialog.show();
        });

        holder.imageStartStop.setOnClickListener(view -> {
            int index = holder.getBindingAdapterPosition();
            if (index != RecyclerView.NO_POSITION) {
                mMediaController.operatePlayMusic(musicInfo);
                notifyItemChanged(index);
                notifyItemChanged(mSelectedPosition);
            }
        });
    }

    @Override
    public int getItemCount() {
        return mMediaState.musicList.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        public ImageView imageStartStop;
        public ImageView imageDelete;
        public TextView  textMusicName;

        public ViewHolder(View itemView) {
            super(itemView);
            textMusicName = itemView.findViewById(R.id.tv_music_name);
            imageDelete = itemView.findViewById(R.id.iv_delete);
            imageStartStop = itemView.findViewById(R.id.iv_start_stop);
        }
    }
}