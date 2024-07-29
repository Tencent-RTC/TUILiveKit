package com.trtc.uikit.livekit.common.uicomponent.music.view;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.FrameLayout;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.trtc.TRTCCloud;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.music.service.MusicService;

@SuppressLint("ViewConstructor")
public class MusicPanelView extends FrameLayout {
    private final Context      mContext;
    private final MusicService mMusicService;

    public MusicPanelView(Context context, String roomId, TRTCCloud trtcCloud) {
        super(context);
        mContext = context;
        mMusicService = new MusicService(roomId, trtcCloud);
        initView();
    }

    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_music_panel, this, true);
        initMusicListView();
    }

    private void initMusicListView() {
        RecyclerView recycleMusicList = findViewById(R.id.rv_music_list);
        recycleMusicList.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL, false));
        MusicListAdapter adapter = new MusicListAdapter(mContext, mMusicService);
        recycleMusicList.setAdapter(adapter);
    }
}
