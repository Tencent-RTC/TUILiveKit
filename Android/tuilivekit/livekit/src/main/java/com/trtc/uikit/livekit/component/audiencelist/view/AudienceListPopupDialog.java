package com.trtc.uikit.livekit.component.audiencelist.view;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;

import androidx.lifecycle.Observer;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.audiencelist.AudienceListView;
import com.trtc.uikit.livekit.component.audiencelist.store.AudienceListState;
import com.trtc.uikit.livekit.component.audiencelist.view.adapter.AudienceListPanelAdapter;

import java.util.Set;

@SuppressLint("ViewConstructor")
public class AudienceListPopupDialog extends PopupDialog {
    private final Context                                  mContext;
    private       AudienceListPanelAdapter                 mAdapter;
    private final AudienceListState                        mAudienceListState;
    private       ImageView                                mImageBack;
    private       RecyclerView                             mRecycleAudienceList;
    private       AudienceListView.OnUserItemClickListener mOnUserItemClickListener;
    private final Observer<Set<TUIRoomDefine.UserInfo>>    mAudienceListObserver = this::onAudienceListChange;

    public AudienceListPopupDialog(Context context, AudienceListState audienceListState) {
        super(context);
        mContext = context;
        mAudienceListState = audienceListState;
        initView();
    }

    protected void addObserver() {
        mAudienceListState.audienceList.observeForever(mAudienceListObserver);
    }

    protected void removeObserver() {
        mAudienceListState.audienceList.removeObserver(mAudienceListObserver);
    }

    protected void initView() {
        View view = LayoutInflater.from(mContext).inflate(R.layout.livekit_layout_audience_list_panel, null);

        bindViewId(view);
        initImageBackView();
        initAudienceListView();
        setView(view);
    }

    @Override
    public void onAttachedToWindow() {
        super.onAttachedToWindow();

        addObserver();
    }

    @Override
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    public void setOnUserItemClickListener(AudienceListView.OnUserItemClickListener listener) {
        mOnUserItemClickListener = listener;
        if (mAdapter != null) {
            mAdapter.setOnItemClickListener(mOnUserItemClickListener);
        }
    }

    private void bindViewId(View view) {
        mImageBack = view.findViewById(R.id.iv_back);
        mRecycleAudienceList = view.findViewById(R.id.rv_audience_list);
    }

    private void initImageBackView() {
        mImageBack.setOnClickListener(view -> {
            dismiss();
        });
    }

    private void initAudienceListView() {
        mRecycleAudienceList.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL,
                false));
        mAdapter = new AudienceListPanelAdapter(mContext, mAudienceListState);
        mRecycleAudienceList.setAdapter(mAdapter);
        mAdapter.setOnItemClickListener(mOnUserItemClickListener);
    }

    private void onAudienceListChange(Set<TUIRoomDefine.UserInfo> userInfo) {
        mAdapter.updateData();
    }
}
