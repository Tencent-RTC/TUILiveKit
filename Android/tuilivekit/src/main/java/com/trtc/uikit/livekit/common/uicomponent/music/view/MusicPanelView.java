package com.trtc.uikit.livekit.common.uicomponent.music.view;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BottomPanelView;
import com.trtc.uikit.livekit.manager.LiveController;

@SuppressLint("ViewConstructor")
public class MusicPanelView extends BottomPanelView {

    public MusicPanelView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void addObserver() {
    }

    @Override
    protected void removeObserver() {
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_music_panel, this, true);

        initMusicListView();
    }

    private void initMusicListView() {
        RecyclerView recycleMusicList = findViewById(R.id.rv_music_list);
        recycleMusicList.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL, false));
        MusicListAdapter adapter = new MusicListAdapter(mContext, mLiveController);
        recycleMusicList.setAdapter(adapter);
    }
}
