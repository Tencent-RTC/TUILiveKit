package com.trtc.uikit.livekit.common.uicomponent.audioeffect;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveController;
import com.trtc.uikit.livekit.common.view.BottomPanelView;

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
        View rootView = LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_music_panel, this, true);
        RecyclerView recycleMusicList = rootView.findViewById(R.id.rv_music_list);
        recycleMusicList.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL, false));
        MusicListAdapter adapter = new MusicListAdapter(mContext, mLiveController);
        recycleMusicList.setAdapter(adapter);
    }
}
