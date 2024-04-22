package com.trtc.uikit.livekit.liveroom.view.anchor.component.common;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.LinearLayout;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;

public class MusicPanel extends LinearLayout {

    private Context           mContext;
    private RecyclerView      mRecycleMusicList;
    private MusicListAdapter  mAdapter;
    private LiveRoomInfo      mLiveRoomInfo;
    private RoomEngineService mRoomEngineService;

    public MusicPanel(Context context, LiveRoomInfo roomInfo, RoomEngineService service) {
        super(context);
        mContext = context;
        mLiveRoomInfo = roomInfo;
        mRoomEngineService = service;
        init();
    }

    public void refresh() {
        mAdapter.notifyDataSetChanged();
    }

    private void init() {
        View rootView = LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_music_panel, this,
                true);

        mRecycleMusicList = rootView.findViewById(R.id.rv_music_list);
        mRecycleMusicList.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL, false));

        mAdapter = new MusicListAdapter(mContext, mRoomEngineService);
        mRecycleMusicList.setAdapter(mAdapter);
    }
}
