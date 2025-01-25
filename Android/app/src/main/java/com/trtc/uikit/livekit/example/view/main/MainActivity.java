package com.trtc.uikit.livekit.example.view.main;

import android.content.Intent;
import android.os.Bundle;
import android.widget.ImageView;

import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.example.BaseActivity;
import com.trtc.uikit.livekit.example.R;
import com.trtc.uikit.livekit.example.store.AppStore;
import com.trtc.uikit.livekit.example.view.main.adapter.MainAdapter;
import com.trtc.uikit.livekit.example.view.main.model.MainItemData;
import com.trtc.uikit.livekit.example.view.main.model.MainTypeEnum;
import com.trtc.uikit.livekit.example.view.me.MeActivity;

import java.util.ArrayList;
import java.util.List;

public class MainActivity extends BaseActivity {

    private final List<MainItemData> mDataList = new ArrayList<>();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.app_activity_main);

        initUserAvatarView();
        initMainRecyclerView();
    }

    private void initUserAvatarView() {
        ImageView ivUserAvatar = findViewById(R.id.iv_avatar);
        ImageLoader.load(getApplicationContext(), ivUserAvatar, AppStore.userAvatar, R.drawable.app_avatar);
        ivUserAvatar.setOnClickListener(v -> {
            Intent intent = new Intent(this, MeActivity.class);
            startActivity(intent);
        });
    }

    private void initMainRecyclerView() {
        mDataList.add(new MainItemData(MainTypeEnum.TYPE_VIDEO_LIVE, R.drawable.app_ic_main_video_live,
                R.string.app_main_item_video_live, R.string.app_main_item_video_live_sub));
        mDataList.add(new MainItemData(MainTypeEnum.TYPE_VOICE_ROOM, R.drawable.app_ic_main_voice_room,
                R.string.app_main_item_voice_room, R.string.app_main_item_voice_room_sub));
        RecyclerView mRecyclerMainList = findViewById(R.id.rv_main_list);
        GridLayoutManager gridLayoutManager = new GridLayoutManager(getApplicationContext(), 2);
        mRecyclerMainList.setLayoutManager(gridLayoutManager);
        mRecyclerMainList.setAdapter(new MainAdapter(getApplicationContext(), mDataList));
    }


}