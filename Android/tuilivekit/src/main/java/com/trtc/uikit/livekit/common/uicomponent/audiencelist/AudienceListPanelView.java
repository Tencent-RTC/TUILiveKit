package com.trtc.uikit.livekit.common.uicomponent.audiencelist;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.ImageView;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BottomPanelView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.UserState;

import java.util.LinkedHashSet;

@SuppressLint("ViewConstructor")
public class AudienceListPanelView extends BottomPanelView {

    private       AudienceListPanelAdapter                    mAdapter;
    private final LiveController                              mLiveController;
    private final UserState                                   mUserState;
    private final Observer<LinkedHashSet<UserState.UserInfo>> mAudienceListObserver = this::onAudienceListChange;

    public AudienceListPanelView(Context context, LiveController liveController) {
        super(context, liveController);
        mLiveController = liveController;
        mUserState = mLiveController.getUserState();
    }

    @Override
    protected void addObserver() {
        mUserState.userList.observe(mAudienceListObserver);
    }

    @Override
    protected void removeObserver() {
        mUserState.userList.removeObserver(mAudienceListObserver);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_layout_anchor_live_audience_list_panel, this, true);

        initImageBackView();
        initAudienceListView();
    }

    private void initImageBackView() {
        ImageView mImageBack = findViewById(R.id.iv_back);
        mImageBack.setOnClickListener(view -> dismiss());
    }

    private void initAudienceListView() {
        RecyclerView recycleAudienceList = findViewById(R.id.rv_audience_list);
        recycleAudienceList.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL,
                false));
        mAdapter = new AudienceListPanelAdapter(mContext, mLiveController);
        recycleAudienceList.setAdapter(mAdapter);
    }

    private void onAudienceListChange(LinkedHashSet<UserState.UserInfo> userInfos) {
        mAdapter.updateData();
    }
}
