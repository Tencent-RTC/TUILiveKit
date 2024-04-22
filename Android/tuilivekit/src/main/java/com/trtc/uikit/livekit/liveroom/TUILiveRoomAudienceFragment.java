package com.trtc.uikit.livekit.liveroom;

import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.viewpager2.widget.ViewPager2;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.view.audience.component.adapter.AudienceSlideAdapter;

import java.util.ArrayList;
import java.util.List;

public class TUILiveRoomAudienceFragment extends Fragment {
    private static final String TAG = "TUILiveAudienceFragment";

    private final List<LiveRoomInfo> mRoomList = new ArrayList<>();

    public TUILiveRoomAudienceFragment(String roomId) {
        LiveRoomInfo currentRoom = new LiveRoomInfo(roomId);
        mRoomList.add(currentRoom);
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View contentView = inflater.inflate(R.layout.livekit_fragment_audience, container, false);

        initView(contentView);
        return contentView;
    }

    private void initView(View rootView) {
        ViewPager2 viewPager = rootView.findViewById(R.id.view_pager2);
        AudienceSlideAdapter adapter = new AudienceSlideAdapter(getActivity().getSupportFragmentManager(),
                getActivity().getLifecycle(), mRoomList);
        viewPager.setAdapter(adapter);

        viewPager.setOrientation(ViewPager2.ORIENTATION_VERTICAL);
//        mViewPager.setOffscreenPageLimit(1);
        viewPager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
                super.onPageScrolled(position, positionOffset, positionOffsetPixels);
            }

            @Override
            public void onPageSelected(int position) {
                super.onPageSelected(position);
                Log.i(TAG, "onPageSelected: position = " + position);
            }

            @Override
            public void onPageScrollStateChanged(int state) {
                super.onPageScrollStateChanged(state);
                Log.i(TAG, "onPageScrollStateChanged: state = " + state);
            }
        });
    }
}
