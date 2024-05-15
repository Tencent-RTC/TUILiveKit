package com.trtc.uikit.livekit.liveroom.view.anchor.component.common;

import static com.trtc.uikit.livekit.liveroom.data.MusicInfo.INVALID_ID;

import android.annotation.SuppressLint;
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
import com.trtc.uikit.livekit.liveroom.core.LiveKitStore;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.data.MusicInfo;

public class MusicListAdapter extends RecyclerView.Adapter<MusicListAdapter.ViewHolder> {

    private final Context           mContext;
    private final RoomEngineService mRoomEngineService;

    public MusicListAdapter(Context context, RoomEngineService service) {
        mContext = context;
        mRoomEngineService = service;
        initData();
    }

    private void initData() {
        if (LiveKitStore.sharedInstance().musicList.isEmpty()) {
            LiveKitStore.sharedInstance().musicList.add(new MusicInfo(1,
                    mContext.getString(R.string.livekit_music_cheerful),
                    "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/PositiveHappyAdvertising.mp3"));
            LiveKitStore.sharedInstance().musicList.add(new MusicInfo(2,
                    mContext.getString(R.string.livekit_music_melancholy),
                    "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/SadCinematicPiano.mp3"));
            LiveKitStore.sharedInstance().musicList.add(new MusicInfo(3,
                    mContext.getString(R.string.livekit_music_wonder_world),
                    "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/WonderWorld.mp3"));
        }
    }

    @NonNull
    @Override
    public MusicListAdapter.ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(
                R.layout.livekit_recycle_item_music, parent, false);
        return new ViewHolder(view);
    }

    @SuppressLint("NotifyDataSetChanged")
    @Override
    public void onBindViewHolder(MusicListAdapter.ViewHolder holder, int position) {
        holder.textMusicName.setText(LiveKitStore.sharedInstance().musicList.get(position).name);
        if (LiveKitStore.sharedInstance().musicList.get(position).isPlaying.get()) {
            holder.imageStartStop.setImageResource(R.drawable.livekit_music_pause);
        } else {
            holder.imageStartStop.setImageResource(R.drawable.livekit_music_start);
        }

        holder.imageDelete.setTag(position);
        holder.imageDelete.setOnClickListener(view -> {
            final int index = (Integer) view.getTag();
            MusicInfo currentItem = LiveKitStore.sharedInstance().musicList.get(index);
            ConfirmWithCheckboxDialog dialog = new ConfirmWithCheckboxDialog(mContext);
            dialog.setTitle(mContext.getString(R.string.livekit_tips_title));
            dialog.setContent(mContext.getString(R.string.livekit_musuic_delete_tips, currentItem.name));
            dialog.setNegativeText(mContext.getString(R.string.livekit_cancel), negativeView -> dialog.dismiss());
            dialog.setPositiveText(mContext.getString(R.string.livekit_confirm), positiveView -> {
                if (currentItem.isPlaying.get()) {
                    mRoomEngineService.stopMusic(currentItem.id);
                    notifyDataSetChanged();
                    LiveKitStore.sharedInstance().currentMusicInfo.set(new MusicInfo());
                }
                LiveKitStore.sharedInstance().musicList.remove(index);
                notifyDataSetChanged();
                dialog.dismiss();
            });
            dialog.show();
        });

        holder.imageStartStop.setTag(position);
        holder.imageStartStop.setOnClickListener(view -> {
            int index = (Integer) view.getTag();
            MusicInfo currentItem = LiveKitStore.sharedInstance().musicList.get(index);
            if (LiveKitStore.sharedInstance().currentMusicInfo.get().id != INVALID_ID
                    && LiveKitStore.sharedInstance().currentMusicInfo.get().id != currentItem.id) {
                mRoomEngineService.stopMusic(LiveKitStore.sharedInstance().currentMusicInfo.get().id);
                LiveKitStore.sharedInstance().currentMusicInfo.get().isPlaying.set(false);
                notifyDataSetChanged();
            }
            LiveKitStore.sharedInstance().currentMusicInfo.set(currentItem);
            if (LiveKitStore.sharedInstance().currentMusicInfo.get().isPlaying.get()) {
                mRoomEngineService.stopMusic(LiveKitStore.sharedInstance().currentMusicInfo.get().id);
                LiveKitStore.sharedInstance().currentMusicInfo.get().isPlaying.set(false);
                notifyDataSetChanged();
            } else {
                mRoomEngineService.startMusic(LiveKitStore.sharedInstance().currentMusicInfo.get());
                LiveKitStore.sharedInstance().currentMusicInfo.get().isPlaying.set(true);
                notifyDataSetChanged();
            }
        });
    }

    @Override
    public int getItemCount() {
        return LiveKitStore.sharedInstance().musicList.size();
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