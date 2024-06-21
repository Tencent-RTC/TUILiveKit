package com.trtc.uikit.livekit.common.uicomponent.audiencelist;

import static com.trtc.uikit.livekit.common.utils.Constants.ROOM_MAX_SHOW_USER_COUNT;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Point;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BottomPanel;
import com.trtc.uikit.livekit.common.view.BottomPanelView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.UserState;

import java.util.LinkedHashSet;

@SuppressLint("ViewConstructor")
public class AudienceListView extends BottomPanelView {

    private       RecyclerView                                mRecycleAudienceList;
    private       AudienceListIconAdapter                     mAdapter;
    private       TextView                                    mTextAudienceCount;
    private       LinearLayout                                mLayoutAudienceCount;
    private       BottomPanel                                 mAudienceListPanel;
    private final Observer<LinkedHashSet<UserState.UserInfo>> mAudienceListObserver = this::onAudienceListChange;
    private final Observer<Integer>                           mMemberCountObserver  = this::onAudienceCountChange;

    public AudienceListView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(
                R.layout.livekit_layout_anchor_live_audience_list_icon, this, true);
        bindViewId();

        initAudienceCountView();
        initAudienceAvatarView();
    }

    private void initAudienceCountView() {
        mLayoutAudienceCount.setOnClickListener(view -> showAudienceListPanelView());
    }

    private void bindViewId() {
        mLayoutAudienceCount = findViewById(R.id.ll_audience_count);
        mTextAudienceCount = findViewById(R.id.tv_audience_count);
        mRecycleAudienceList = findViewById(R.id.rv_audience_list);
    }

    @Override
    protected void addObserver() {
        mUserState.userList.observe(mAudienceListObserver);
        mRoomState.userCount.observe(mMemberCountObserver);
    }

    @Override
    protected void removeObserver() {
        mUserState.userList.removeObserver(mAudienceListObserver);
        mRoomState.userCount.removeObserver(mMemberCountObserver);
    }

    @SuppressLint("ClickableViewAccessibility")
    private void initAudienceAvatarView() {
        mRecycleAudienceList.setLayoutManager(new LinearLayoutManager(getContext(), LinearLayoutManager.HORIZONTAL,
                false));
        mAdapter = new AudienceListIconAdapter(mContext, mLiveController);
        mRecycleAudienceList.setAdapter(mAdapter);
        mRecycleAudienceList.setOnTouchListener(new OnTouchListener() {
            private final Point point = new Point();
            private boolean scroll = false;

            @Override
            public boolean onTouch(View v, MotionEvent event) {
                switch (event.getAction()) {
                    case MotionEvent.ACTION_DOWN:
                        point.set((int) event.getX(), (int) event.getY());
                        scroll = false;
                        break;
                    case MotionEvent.ACTION_MOVE:
                        if (Math.abs(event.getX() - point.x) > 10 && !scroll) {
                            scroll = true;
                        }
                        break;
                    case MotionEvent.ACTION_UP:
                        if (!scroll) {
                            showAudienceListPanelView();
                        }
                        break;
                    default:
                        break;
                }
                return false;
            }
        });
    }

    private void showAudienceListPanelView() {
        if (mAudienceListPanel == null) {
            AudienceListPanelView panelView = new AudienceListPanelView(mContext, mLiveController);
            mAudienceListPanel = BottomPanel.create(panelView);
            panelView.setOnBackButtonClickListener(() -> mAudienceListPanel.dismiss());
        }
        mAudienceListPanel.show();
    }

    private void onAudienceCountChange(int memberCount) {
        setUserCount(memberCount);
    }

    private void onAudienceListChange(LinkedHashSet<UserState.UserInfo> userInfos) {
        setUserCount(mRoomState.userCount.get());
        mAdapter.updateData();
    }

    @SuppressLint("SetTextI18n")
    private void setUserCount(int count) {
        if (mUserState.userList.get().size() > ROOM_MAX_SHOW_USER_COUNT) {
            mTextAudienceCount.setText("" + count);
        } else {
            mTextAudienceCount.setText("" + mUserState.userList.get().size());
        }
    }
}