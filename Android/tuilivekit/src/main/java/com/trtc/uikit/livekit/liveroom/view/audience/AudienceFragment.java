package com.trtc.uikit.livekit.liveroom.view.audience;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.core.EngineManager;
import com.trtc.uikit.livekit.liveroom.core.LiveKitStore;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.core.TUILiveDefine;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;

public class AudienceFragment extends Fragment {

    private final String            mRoomId;
    private       AudienceView      mAudienceView;
    private       LiveRoomInfo      mLiveRoomInfo;
    private       RoomEngineService mRoomEngineService;

    public AudienceFragment(String roomId) {
        mRoomId = roomId;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        initEngine(mRoomId);
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View contentView = inflater.inflate(R.layout.livekit_fragment_audience_item, container, false);
        RelativeLayout layoutContainer = contentView.findViewById(R.id.rl_container);
        mAudienceView = new AudienceView(getActivity(), mLiveRoomInfo, mRoomEngineService);
        layoutContainer.addView(mAudienceView);
        mAudienceView.updateStatus(AudienceView.AudienceViewStatus.CREATE);
        addObserver();
        return contentView;
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        mAudienceView.updateStatus(AudienceView.AudienceViewStatus.START_DISPLAY);
    }

    @Override
    public void onResume() {
        super.onResume();
        mAudienceView.updateStatus(AudienceView.AudienceViewStatus.DISPLAY_COMPLETE);
    }

    @Override
    public void onPause() {
        super.onPause();
        mAudienceView.updateStatus(AudienceView.AudienceViewStatus.END_DISPLAY);
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        removeObserver();
        mAudienceView.updateStatus(AudienceView.AudienceViewStatus.DESTROY);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        unInitEngine();
    }

    private void initEngine(String roomId) {
        LiveKitStore.sharedInstance().selfInfo.userId = TUILogin.getUserId();
        LiveKitStore.sharedInstance().selfInfo.name.set(TUILogin.getNickName());
        LiveKitStore.sharedInstance().selfInfo.avatarUrl.set(TUILogin.getFaceUrl());
        LiveKitStore.sharedInstance().selfInfo.role.set(TUILiveDefine.RoleType.AUDIENCE);

        mLiveRoomInfo = new LiveRoomInfo(roomId);
        mLiveRoomInfo.anchorInfo.userId = roomId;
        mRoomEngineService = new RoomEngineService(mLiveRoomInfo);
        EngineManager.sharedInstance().mRoomEngineMap.put(roomId, mRoomEngineService);
    }

    private void unInitEngine() {
        mRoomEngineService.destroy();
    }

    private void addObserver() {
    }

    private void removeObserver() {
    }
}

