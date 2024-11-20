package com.trtc.uikit.livekit.component.music;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.music.service.MusicService;
import com.trtc.uikit.livekit.component.music.view.MusicListAdapter;

@SuppressLint("ViewConstructor")
public class MusicPanelView extends FrameLayout {
    private final Context      mContext;
    private       MusicService mMusicService;

    public MusicPanelView(Context context) {
        super(context);
        mContext = context;
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_music_panel, this, true);
    }

    public MusicPanelView(Context context, AttributeSet attrs) {
        super(context, attrs);
        mContext = context;
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_music_panel, this, true);
    }

    public MusicPanelView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_music_panel, this, true);
    }

    public void init(String roomId) {
        mMusicService = new MusicService(roomId);
        initMusicListView();
    }

    private void initMusicListView() {
        RecyclerView recycleMusicList = findViewById(R.id.rv_music_list);
        recycleMusicList.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL, false));
        MusicListAdapter adapter = new MusicListAdapter(mContext, mMusicService);
        recycleMusicList.setAdapter(adapter);
    }
}
