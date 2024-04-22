package com.trtc.uikit.livekit.liveroom.view.anchor.component.livestreaming;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.LinearLayout;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.core.LiveKitStore;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.data.UserInfo;

import java.util.LinkedHashSet;
import java.util.concurrent.CopyOnWriteArraySet;

@SuppressLint("ViewConstructor")
public class AnchorLinkMicPanel extends LinearLayout {

    private       AnchorLinkMicPanelAdapter               mAdapter;
    private final Context                                 mContext;
    private final LiveRoomInfo                            mLiveRoomInfo;
    private final RoomEngineService                       mRoomEngineService;
    private final PopupDialog.DialogActionListener        mDismissListener;
    private final Observer<CopyOnWriteArraySet<UserInfo>> mLinkingAudienceListObserver   = (audienceList) -> {
        mAdapter.updateData();
    };
    private final Observer<LinkedHashSet<UserInfo>>       mApplyLinkAudienceListObserver = (audienceList) -> {
        mAdapter.updateData();
    };

    public AnchorLinkMicPanel(Context context, LiveRoomInfo roomInfo, RoomEngineService service,
                              PopupDialog.DialogActionListener listener) {
        super(context);
        mContext = context;
        mLiveRoomInfo = roomInfo;
        mRoomEngineService = service;
        mDismissListener = listener;
        init();
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        Log.i("AnchorLinkMicPanel", "onAttachedToWindow");
        mAdapter.updateData();
        addObserver();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        Log.i("AnchorLinkMicPanel", "onDetachedFromWindow");
        removeObserver();
    }

    private void addObserver() {
        mLiveRoomInfo.linkingAudienceList.observe(mLinkingAudienceListObserver);
        LiveKitStore.sharedInstance().applyLinkAudienceList.observe(mApplyLinkAudienceListObserver);
    }

    private void removeObserver() {
        mLiveRoomInfo.linkingAudienceList.removeObserver(mLinkingAudienceListObserver);
        LiveKitStore.sharedInstance().applyLinkAudienceList.removeObserver(mApplyLinkAudienceListObserver);
    }

    private void init() {
        View rootView = LayoutInflater.from(mContext).inflate(R.layout.livekit_layout_anchor_link_mic_panel, this,
                true);

        RecyclerView recycleAudienceList = rootView.findViewById(R.id.rv_link_user_list);
        recycleAudienceList.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL,
                false));

        mAdapter = new AnchorLinkMicPanelAdapter(mContext, mLiveRoomInfo, mRoomEngineService);
        recycleAudienceList.setAdapter(mAdapter);

        findViewById(R.id.iv_back).setOnClickListener(view -> {
            if (mDismissListener != null) {
                mDismissListener.dismiss();
            }
        });
    }

}

