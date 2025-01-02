package com.trtc.uikit.component.music;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.uikit.component.common.DataReporter;
import com.trtc.uikit.component.music.service.MusicService;
import com.trtc.uikit.component.music.view.MusicListAdapter;

@SuppressLint("ViewConstructor")
public class MusicPanelView extends FrameLayout {
    private final Context      mContext;
    private       MusicService mMusicService;

    private static final int LIVEKIT_METRICS_PANEL_SHOW_LIVE_ROOM_MUSIC  = 190018;
    private static final int LIVEKIT_METRICS_PANEL_SHOW_VOICE_ROOM_MUSIC = 191016;

    public MusicPanelView(Context context) {
        this(context, null);
    }

    public MusicPanelView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public MusicPanelView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_music_panel, this, true);
    }

    public void init(String roomId) {
        mMusicService = new MusicService(roomId);
        initMusicListView();
        reportData(roomId);
    }

    private void initMusicListView() {
        RecyclerView recycleMusicList = findViewById(R.id.rv_music_list);
        recycleMusicList.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL, false));
        MusicListAdapter adapter = new MusicListAdapter(mContext, mMusicService);
        recycleMusicList.setAdapter(adapter);
    }

    private void reportData(String roomId) {
        boolean isVoiceRoom = !TextUtils.isEmpty(roomId) && roomId.startsWith("voice_");
        if (isVoiceRoom) {
            DataReporter.reportEventData(LIVEKIT_METRICS_PANEL_SHOW_VOICE_ROOM_MUSIC);
        } else {
            DataReporter.reportEventData(LIVEKIT_METRICS_PANEL_SHOW_LIVE_ROOM_MUSIC);
        }

    }
}
