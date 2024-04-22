package com.trtc.uikit.livekit.voiceroom.view.topview;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.TextView;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveController;
import com.trtc.uikit.livekit.common.core.store.state.operation.UserState;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.voiceroom.view.panel.audiencelist.AudienceListPanel;

import java.util.LinkedHashSet;

@SuppressLint("ViewConstructor")
public class AudienceListView extends BasicView {

    private RecyclerView            mRecycleAudienceList;
    private AudienceListIconAdapter mAdapter;
    private TextView                mTextAudienceCount;

    private final Observer<LinkedHashSet<UserState.UserInfo>> mAudienceListObserver =
            (audienceList) -> mAdapter.updateData();

    private final Observer<Integer> mMemberCountObserver = (memberCount)
            -> mTextAudienceCount.setText(getContext().getString(R.string.livekit_audience_number, memberCount));

    public AudienceListView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        View rootView = LayoutInflater.from(mContext).inflate(
                R.layout.livekit_layout_anchor_live_audience_list_icon, this, true);
        mTextAudienceCount = rootView.findViewById(R.id.tv_audience_count);
        mRecycleAudienceList = rootView.findViewById(R.id.rv_audience_list);
        initAudienceList();
        updateAudienceCount(mRoomState.audienceCount.get());
    }

    @Override
    protected void addObserver() {
        mUserState.audienceList.observe(mAudienceListObserver);
        mRoomState.audienceCount.observe(mMemberCountObserver);
    }

    @Override
    protected void removeObserver() {
        mUserState.audienceList.removeObserver(mAudienceListObserver);
        mRoomState.audienceCount.removeObserver(mMemberCountObserver);
    }

    private void initAudienceList() {
        mRecycleAudienceList.setLayoutManager(new LinearLayoutManager(getContext(), LinearLayoutManager.HORIZONTAL,
                false));
        mAdapter = new AudienceListIconAdapter(mContext, mLiveController);
        mRecycleAudienceList.setAdapter(mAdapter);
        setOnClickListener(view -> {
            AudienceListPanel panel = new AudienceListPanel(mContext, mLiveController);
            panel.show();
        });
    }

    private void updateAudienceCount(int memberCount) {
        mTextAudienceCount.setText(mContext.getString(R.string.livekit_audience_number, memberCount));
    }
}