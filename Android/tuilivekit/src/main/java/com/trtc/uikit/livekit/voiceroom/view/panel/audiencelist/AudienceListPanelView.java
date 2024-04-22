package com.trtc.uikit.livekit.voiceroom.view.panel.audiencelist;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.ImageView;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveController;
import com.trtc.uikit.livekit.common.core.store.state.operation.UserState;
import com.trtc.uikit.livekit.common.view.BasicView;

import java.util.LinkedHashSet;

@SuppressLint("ViewConstructor")
public class AudienceListPanelView extends BasicView {
    private final LiveController mLiveController;
    private final UserState      mUserState;

    private AudienceListPanelAdapter  mAdapter;
    private OnBackButtonClickListener mOnBackButtonClickListener;

    private final Observer<LinkedHashSet<UserState.UserInfo>> mAudienceListObserver =
            (audienceList) -> mAdapter.updateData();

    public AudienceListPanelView(Context context, LiveController liveController) {
        super(context, liveController);
        mLiveController = liveController;
        mUserState = mLiveController.getUserState();
    }

    @Override
    protected void addObserver() {
        mUserState.audienceList.observe(mAudienceListObserver);
    }

    @Override
    protected void removeObserver() {
        mUserState.audienceList.removeObserver(mAudienceListObserver);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_layout_anchor_live_audience_list_panel, this, true);
        RecyclerView recycleAudienceList = findViewById(R.id.rv_audience_list);
        recycleAudienceList.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL,
                false));
        mAdapter = new AudienceListPanelAdapter(mContext, mLiveController);
        recycleAudienceList.setAdapter(mAdapter);
        ImageView mImageBack = findViewById(R.id.iv_back);
        mImageBack.setOnClickListener(view -> {
            if (mOnBackButtonClickListener != null) {
                mOnBackButtonClickListener.onClick();
            }
        });
    }

    public void setOnBackButtonClickListener(OnBackButtonClickListener listener) {
        mOnBackButtonClickListener = listener;
    }

    public interface OnBackButtonClickListener {
        void onClick();
    }
}
