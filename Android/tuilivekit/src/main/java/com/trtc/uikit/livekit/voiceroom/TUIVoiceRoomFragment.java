package com.trtc.uikit.livekit.voiceroom;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveDefine;
import com.trtc.uikit.livekit.common.core.LiveController;
import com.trtc.uikit.livekit.common.core.controller.ViewController;
import com.trtc.uikit.livekit.common.core.store.LiveStore;
import com.trtc.uikit.livekit.common.core.store.state.operation.RoomState;
import com.trtc.uikit.livekit.common.core.store.state.operation.UserState;
import com.trtc.uikit.livekit.common.core.store.state.view.ViewState;
import com.trtc.uikit.livekit.voiceroom.view.RootView;

public class TUIVoiceRoomFragment extends Fragment {
    private final String                  mRoomId;
    private final LiveDefine.RoomBehavior mRoomBehavior;
    private final LiveDefine.RoomParams   mRoomParams;

    private RelativeLayout mLayoutContainer;
    private RootView       mRootView;
    private LiveController mLiveController;
    private ViewState      mViewState;
    private RoomState      mRoomState;
    private UserState      mUserState;

    private final OnBackPressedCallback mBackPressedCallback = new OnBackPressedCallback(true) {
        @Override
        public void handleOnBackPressed() {
            if (mViewState.showEndView.get()) {
                requireActivity().finish();
            } else {
                mLiveController.getViewController().finish();
            }
        }
    };

    private final Observer<ViewState.NavigationState> mRouteObserver = route -> {
        if (route == ViewState.NavigationState.EXIT) {
            exit();
        }
    };

    private final Observer<RoomState.EnterRoomSate> mEnterRoomSateObserver = state -> {
        if (state == RoomState.EnterRoomSate.IN_ROOM) {
            mLiveController.getUserController().getAudienceList();
            mLiveController.getUserController().updateOwnerUserInfo();
            mLiveController.getSeatController().getSeatList();
            if (mUserState.selfInfo.role.get() == TUIRoomDefine.Role.ROOM_OWNER) {
                mLiveController.getSeatController().getSeatApplicationList();
            }
        }
    };

    public TUIVoiceRoomFragment(String roomId, LiveDefine.RoomBehavior behavior, LiveDefine.RoomParams params) {
        mRoomId = roomId;
        mRoomBehavior = behavior;
        mRoomParams = params;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        init();
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View contentView = inflater.inflate(R.layout.livekit_voiceroom_fragment_main, container, false);
        mLayoutContainer = contentView.findViewById(R.id.rl_container);
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        if (mRootView == null) {
            mRootView = new RootView(requireActivity(), mLiveController);
        }
        mLayoutContainer.addView(mRootView, layoutParams);
        addObserver();
        requireActivity().getOnBackPressedDispatcher().addCallback(getViewLifecycleOwner(), mBackPressedCallback);
        return contentView;
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        removeObserver();
        mBackPressedCallback.remove();
        mLayoutContainer.removeView(mRootView);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        LiveStore.destroyInstance();
    }

    public void enableBackPressedCallBack(boolean enable) {
        mBackPressedCallback.setEnabled(enable);
    }

    private void init() {
        mLiveController = LiveStore.sharedInstance().getLiveController();
        mViewState = mLiveController.getViewState();
        mRoomState = mLiveController.getRoomSate();
        mUserState = mLiveController.getUserState();
        switch (mRoomBehavior) {
            case AUTO_CREATE:
                mLiveController.getRoomController().start(mRoomId, mRoomParams);
                break;
            case JOIN:
                mLiveController.getRoomController().join(mRoomId);
                break;
            case PREPARE_CREATE:
                mLiveController.getRoomController().setRoomParams(mRoomId, mRoomParams);
                mLiveController.getViewController().setShowAnchorPreview(true);
                break;
            default:
                break;
        }
    }

    private void addObserver() {
        mViewState.currentNavigationState.observe(mRouteObserver);
        mRoomState.enterRoomState.observe(mEnterRoomSateObserver);
    }

    private void removeObserver() {
        mViewState.currentNavigationState.removeObserver(mRouteObserver);
        mRoomState.enterRoomState.removeObserver(mEnterRoomSateObserver);
    }

    private void exit() {
        ViewController viewController = mLiveController.getViewController();
        if (mUserState.selfInfo.role.get() == TUIRoomDefine.Role.ROOM_OWNER) {
            viewController.setShowEndView(true);
        } else {
            requireActivity().finish();
        }
        mLiveController.getRoomController().exit();
    }
}
