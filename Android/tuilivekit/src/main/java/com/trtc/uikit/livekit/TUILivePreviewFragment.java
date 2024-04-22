package com.trtc.uikit.livekit;

import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_START_LIVE_ROOM;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_START_VOICE_ROOM;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.google.android.material.tabs.TabLayout;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.uikit.livekit.common.core.LiveDefine;
import com.trtc.uikit.livekit.common.core.store.LiveStore;
import com.trtc.uikit.livekit.liveroom.TUILiveRoomAnchorFragment;
import com.trtc.uikit.livekit.voiceroom.TUIVoiceRoomFragment;

import java.util.Map;

public class TUILivePreviewFragment extends Fragment implements ITUINotification {
    private TabLayout                 mLayoutPreview;
    private FrameLayout               mLayoutLiveRoom;
    private FrameLayout               mLayoutVoiceRoom;
    private TUIVoiceRoomFragment      mTUIVoiceRoomFragment;
    private TUILiveRoomAnchorFragment mTUILiveRoomAnchorFragment;

    private final String mLiveRoomId;
    private final String mVoiceRoomId;

    public TUILivePreviewFragment(String liveRoomId, String voiceRoomId) {
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_START_LIVE_ROOM, this);
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_START_VOICE_ROOM, this);
        mLiveRoomId = liveRoomId;
        mVoiceRoomId = voiceRoomId;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        FragmentManager fragmentManager = requireActivity().getSupportFragmentManager();
        FragmentTransaction liveRoomTransaction = fragmentManager.beginTransaction();
        mTUILiveRoomAnchorFragment = new TUILiveRoomAnchorFragment(mLiveRoomId);
        liveRoomTransaction.add(R.id.fl_live_room, mTUILiveRoomAnchorFragment);
        liveRoomTransaction.commit();

        LiveDefine.RoomParams params = new LiveDefine.RoomParams();
        params.maxSeatCount = 8;
        params.seatMode = TUIRoomDefine.SeatMode.APPLY_TO_TAKE;
        params.roomName = TUILogin.getNickName();
        mTUIVoiceRoomFragment = new TUIVoiceRoomFragment(mVoiceRoomId, LiveDefine.RoomBehavior.PREPARE_CREATE, params);
        FragmentTransaction voiceRoomTransaction = fragmentManager.beginTransaction();
        voiceRoomTransaction.add(R.id.fl_voice_room, mTUIVoiceRoomFragment);
        voiceRoomTransaction.commit();
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View contentView = inflater.inflate(R.layout.livekit_fragment_preview, container, false);
        initView(contentView);
        return contentView;
    }

    private void initView(View rootView) {
        mLayoutPreview = rootView.findViewById(R.id.tl_tab);
        mLayoutLiveRoom = rootView.findViewById(R.id.fl_live_room);
        mLayoutVoiceRoom = rootView.findViewById(R.id.fl_voice_room);
        mTUIVoiceRoomFragment.enableBackPressedCallBack(false);
        mTUILiveRoomAnchorFragment.enableBackPressedCallBack(true);
        mLayoutPreview.addOnTabSelectedListener(new TabLayout.OnTabSelectedListener() {
            @Override
            public void onTabSelected(TabLayout.Tab tab) {
                if (tab.getPosition() == 0) {
                    mLayoutVoiceRoom.setVisibility(View.GONE);
                    mLayoutLiveRoom.setVisibility(View.VISIBLE);
                    mTUIVoiceRoomFragment.enableBackPressedCallBack(false);
                    mTUILiveRoomAnchorFragment.enableBackPressedCallBack(true);
                } else {
                    mLayoutLiveRoom.setVisibility(View.GONE);
                    mLayoutVoiceRoom.setVisibility(View.VISIBLE);
                    mTUILiveRoomAnchorFragment.enableBackPressedCallBack(false);
                    mTUIVoiceRoomFragment.enableBackPressedCallBack(true);
                }
            }

            @Override
            public void onTabUnselected(TabLayout.Tab tab) {

            }

            @Override
            public void onTabReselected(TabLayout.Tab tab) {

            }
        });
    }

    @Override
    public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {
        if (EVENT_SUB_KEY_START_LIVE_ROOM.equals(subKey) || EVENT_SUB_KEY_START_VOICE_ROOM.equals(subKey)) {
            mLayoutPreview.setVisibility(View.GONE);
        }
        if (EVENT_SUB_KEY_START_VOICE_ROOM.equals(subKey)) {
            LiveStore.sharedInstance().getLiveController().getMediaController().closeLocalCamera();
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        TUICore.unRegisterEvent(this);
    }
}