package com.trtc.uikit.livekit.component.roomlist;

import static androidx.viewpager2.widget.ViewPager2.SCROLL_STATE_IDLE;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_SUB_KEY_LINK_STATUS_CHANGE;
import static com.trtc.uikit.livekit.voiceroom.view.TUIVoiceRoomFragment.RoomBehavior.JOIN;

import android.annotation.SuppressLint;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.lifecycle.Lifecycle;
import androidx.viewpager2.adapter.FragmentStateAdapter;
import androidx.viewpager2.widget.ViewPager2;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.LiveIdentityGenerator;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.roomlist.service.RoomListService;
import com.trtc.uikit.livekit.component.roomlist.store.RoomListState;
import com.trtc.uikit.livekit.livestream.view.audience.TUILiveRoomAudienceFragment;
import com.trtc.uikit.livekit.LiveIdentityGenerator.RoomType;
import com.trtc.uikit.livekit.voiceroom.view.TUIVoiceRoomFragment;

import java.util.List;
import java.util.Map;

public class TUILiveAudienceFragment extends Fragment implements ITUINotification {

    private       AudienceSlideAdapter     mViewPagerAdapter;
    private       ViewPager2               mViewPager2;
    private final RoomListService          mRoomListService  = new RoomListService();
    private final RoomListState            mRoomListState    = mRoomListService.mRoomListState;
    private final Observer<List<LiveInfo>> mLiveListObserver = this::onLiveListChange;

    public TUILiveAudienceFragment(String roomId) {
        LiveInfo firstLiveInfo = new LiveInfo();
        firstLiveInfo.roomInfo = new TUIRoomDefine.RoomInfo();
        firstLiveInfo.roomInfo.roomId = roomId;
        mRoomListState.mLiveList.add(firstLiveInfo);
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View contentView = inflater.inflate(R.layout.livekit_fragment_audience, container, false);

        initView(contentView);
        return contentView;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        addObserver();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        removeObserver();
    }

    private void addObserver() {
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_LINK_STATUS_CHANGE, this);
        mRoomListState.mLiveList.observe(mLiveListObserver);
    }

    private void removeObserver() {
        TUICore.unRegisterEvent(this);
        mRoomListState.mLiveList.removeObserver(mLiveListObserver);
    }

    private void post(Runnable task) {
        if (getView() == null) {
            return;
        }
        getView().post(task);
    }

    private void initView(View rootView) {
        mViewPager2 = rootView.findViewById(R.id.view_pager2);
        AudienceSlideAdapter adapter = new AudienceSlideAdapter(requireActivity().getSupportFragmentManager(),
                requireActivity().getLifecycle(), mRoomListState.mLiveList.get());
        mViewPagerAdapter = adapter;
        mViewPager2.setAdapter(adapter);

        mViewPager2.setOrientation(ViewPager2.ORIENTATION_VERTICAL);
        mViewPager2.setOffscreenPageLimit(1);
        mViewPager2.setUserInputEnabled(true);
        mViewPager2.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageScrollStateChanged(int state) {
                if (state == SCROLL_STATE_IDLE
                        && mViewPager2.getCurrentItem() == adapter.getItemCount() - 1
                        && !TextUtils.isEmpty(mRoomListState.mFetchListCursor)) {
                    // mRoomListService.fetchLiveList(true);
                }
            }
        });
        // mRoomListService.fetchLiveList(true);
    }

    @Override
    public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {
        if (EVENT_SUB_KEY_LINK_STATUS_CHANGE.equals(subKey) && mViewPager2 != null && param != null) {
            Boolean enableSlide = (Boolean) param.get("EVENT_PARAMS_KEY_ENABLE_SLIDE");
            if (enableSlide != null) {
                mViewPager2.setUserInputEnabled(enableSlide);
            }
        }
    }

    @SuppressLint("NotifyDataSetChanged")
    private void onLiveListChange(List<LiveInfo> liveList) {
        post(() -> mViewPagerAdapter.notifyDataSetChanged());
    }

    static class AudienceSlideAdapter extends FragmentStateAdapter {
        private final List<LiveInfo>        mRoomList;
        private final LiveIdentityGenerator mRoomIdStrategy = LiveIdentityGenerator.getInstance();

        public AudienceSlideAdapter(@NonNull FragmentManager fragmentManager, @NonNull Lifecycle lifecycle,
                                    List<LiveInfo> roomList) {
            super(fragmentManager, lifecycle);
            mRoomList = roomList;
        }

        @NonNull
        @Override
        public Fragment createFragment(int position) {
            LiveInfo liveInfo = mRoomList.get(position);
            RoomType roomType = mRoomIdStrategy.getIDType(liveInfo.roomInfo.roomId);
            if (roomType == RoomType.VOICE) {
                return new TUIVoiceRoomFragment(liveInfo.roomInfo.roomId, JOIN, null);
            } else {
                return new TUILiveRoomAudienceFragment(liveInfo.roomInfo.roomId);
            }
        }

        @Override
        public int getItemCount() {
            return mRoomList.size();
        }
    }
}
