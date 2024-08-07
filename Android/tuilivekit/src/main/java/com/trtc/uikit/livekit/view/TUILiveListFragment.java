package com.trtc.uikit.livekit.view;


import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;

import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.roomlist.view.RoomListView;

public class TUILiveListFragment extends Fragment {
    private RelativeLayout mLayoutContainer;
    private RoomListView   mRoomListView;

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View contentView = inflater.inflate(R.layout.livekit_fragment_room_list, container, false);
        mLayoutContainer = contentView.findViewById(R.id.rl_container);
        if (mRoomListView == null && getActivity() != null) {
            mRoomListView = new RoomListView(getActivity());
        }
        mLayoutContainer.addView(mRoomListView);
        return contentView;
    }

    @Override
    public void onDestroyView() {
        mLayoutContainer.removeView(mRoomListView);
        super.onDestroyView();
    }
}

