package com.trtc.uikit.livekit.liveroom.view.audience.component.adapter;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.lifecycle.Lifecycle;
import androidx.viewpager2.adapter.FragmentStateAdapter;

import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.view.audience.AudienceFragment;

import java.util.List;

public class AudienceSlideAdapter extends FragmentStateAdapter {
    private final List<LiveRoomInfo> mRoomList;

    public AudienceSlideAdapter(@NonNull FragmentManager fragmentManager, @NonNull Lifecycle lifecycle,
                                List<LiveRoomInfo> roomList) {
        super(fragmentManager, lifecycle);
        mRoomList = roomList;
    }

    @NonNull
    @Override
    public Fragment createFragment(int position) {
        return new AudienceFragment(mRoomList.get(position).roomId);
    }

    @Override
    public int getItemCount() {
        return mRoomList.size();
    }
}
