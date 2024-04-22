package com.trtc.uikit.livekit.liveroom.view.common.audiencelist;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.data.UserInfo;

import java.util.concurrent.CopyOnWriteArraySet;

@SuppressLint("ViewConstructor")
public class AudienceListView extends LinearLayout {

    private final Context                                 mContext;
    private       LinearLayout                            mLinearRoot;
    private       RecyclerView                            mRecycleAudienceList;
    private       AudienceListIconAdapter                 mAdapter;
    private       TextView                                mTextAudienceCount;
    private       PopupDialog                             mAudienceListDialog;
    private       AudienceListPanel                       mAudienceListPanel;
    private final LiveRoomInfo                            mLiveRoomInfo;
    private final RoomEngineService                       mRoomEngineService;
    private final Observer<CopyOnWriteArraySet<UserInfo>> mAudienceListObserver  = (audienceList) ->
            mAdapter.updateData();
    private final Observer<Integer>                       mAudienceCountObserver = (audienceCount) ->
            mTextAudienceCount.setText(getContext().getString(R.string.livekit_audience_number, audienceCount));

    public AudienceListView(Context context, LiveRoomInfo roomInfo, RoomEngineService service) {
        super(context);
        mContext = context;
        mLiveRoomInfo = roomInfo;
        mRoomEngineService = service;
        init();
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        addObserver();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void addObserver() {
        mLiveRoomInfo.audienceList.observe(mAudienceListObserver);
        mLiveRoomInfo.audienceCount.observe(mAudienceCountObserver);
    }

    private void removeObserver() {
        mLiveRoomInfo.audienceList.removeObserver(mAudienceListObserver);
        mLiveRoomInfo.audienceCount.removeObserver(mAudienceCountObserver);
    }

    private void init() {
        View rootView = LayoutInflater.from(mContext).inflate(
                R.layout.livekit_layout_anchor_live_audience_list_icon, this, true);

        mLinearRoot = rootView.findViewById(R.id.ll_root);
        mTextAudienceCount = rootView.findViewById(R.id.tv_audience_count);
        mRecycleAudienceList = rootView.findViewById(R.id.rv_audience_list);

        initAudienceList();
        initAudienceCount();
        initListener();
    }

    private void initAudienceList() {
        mRecycleAudienceList.setLayoutManager(new LinearLayoutManager(getContext(), LinearLayoutManager.HORIZONTAL,
                false));

        mAdapter = new AudienceListIconAdapter(mContext, mLiveRoomInfo);
        mRecycleAudienceList.setAdapter(mAdapter);
    }

    private void initAudienceCount() {
        mTextAudienceCount.setText(mContext.getString(R.string.livekit_audience_number,
                mLiveRoomInfo.audienceCount.get()));
    }

    private void initListener() {
        mLinearRoot.setOnClickListener((view) -> {
            mRoomEngineService.getAudienceList();
            if (mAudienceListDialog == null) {
                mAudienceListDialog = new PopupDialog(getContext());
                mAudienceListDialog.setOnDismissListener((dialogInterface) -> {
                });
            }
            if (mAudienceListPanel == null) {
                mAudienceListPanel = new AudienceListPanel(getContext(), mLiveRoomInfo);
            }
            mAudienceListDialog.setView(mAudienceListPanel);
            mAudienceListDialog.show();
        });
    }
}