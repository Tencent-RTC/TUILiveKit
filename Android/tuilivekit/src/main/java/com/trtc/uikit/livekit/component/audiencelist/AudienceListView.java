package com.trtc.uikit.livekit.component.audiencelist;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Point;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.audiencelist.service.AudienceListObserver;
import com.trtc.uikit.livekit.component.audiencelist.service.AudienceListService;
import com.trtc.uikit.livekit.component.audiencelist.store.AudienceListState;
import com.trtc.uikit.livekit.component.audiencelist.view.AudienceListPopupDialog;
import com.trtc.uikit.livekit.component.audiencelist.view.adapter.AudienceListIconAdapter;

import java.util.LinkedHashSet;

@SuppressLint("ViewConstructor")
public class AudienceListView extends FrameLayout {
    private static final int ROOM_MAX_SHOW_USER_COUNT = 100;
    private final Context mContext;
    private       RecyclerView                                    mRecycleAudienceList;
    private       AudienceListIconAdapter                         mAdapter;
    private       TextView                                        mTextAudienceCount;
    private       LinearLayout                                    mLayoutAudienceCount;
    private       AudienceListObserver                            mAudienceObserver;
    private final AudienceListService                             mAudienceListService
            = new AudienceListService();
    private final AudienceListState                               mAudienceListState
            = mAudienceListService.mAudienceListState;
    private final Observer<LinkedHashSet<TUIRoomDefine.UserInfo>> mAudienceListObserver
            = this::onAudienceListChange;
    private final Observer<Integer>                               mAudienceCountObserver
            = this::onAudienceCountChange;
    private       TUIRoomEngine                                   mRoomEngine;
    private       AudienceListPopupDialog                         mAudienceListPopupDialog;

    public AudienceListView(Context context) {
        this(context, null);
    }

    public AudienceListView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AudienceListView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        LayoutInflater.from(mContext).inflate(R.layout.livekit_layout_anchor_live_audience_list_icon, this,
                true);
    }

    public void init(String roomId) {
        mAudienceListService.initRoomInfo(roomId);
    }

    protected void initView() {
        bindViewId();

        initAudienceCountView();
        initAudienceAvatarView();
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();

        initView();
        addObserver();
        mRoomEngine = TUIRoomEngine.sharedInstance();
        mAudienceObserver = new AudienceListObserver(mAudienceListState);
        mRoomEngine.addObserver(mAudienceObserver);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
        mRoomEngine.removeObserver(mAudienceObserver);
    }

    private void initAudienceCountView() {
        mLayoutAudienceCount.setOnClickListener(view -> showAudienceListPanelView());
    }

    private void bindViewId() {
        mLayoutAudienceCount = findViewById(R.id.ll_audience_count);
        mTextAudienceCount = findViewById(R.id.tv_audience_count);
        mRecycleAudienceList = findViewById(R.id.rv_audience_list);
    }

    protected void addObserver() {
        mAudienceListState.audienceCount.observe(mAudienceCountObserver);
        mAudienceListState.audienceList.observe(mAudienceListObserver);
    }

    protected void removeObserver() {
        mAudienceListState.audienceCount.removeObserver(mAudienceCountObserver);
        mAudienceListState.audienceList.removeObserver(mAudienceListObserver);
    }

    @SuppressLint("ClickableViewAccessibility")
    private void initAudienceAvatarView() {
        mRecycleAudienceList.setLayoutManager(new LinearLayoutManager(getContext(), LinearLayoutManager.HORIZONTAL,
                false));
        mAdapter = new AudienceListIconAdapter(mContext, mAudienceListState);
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
        if (mAudienceListPopupDialog == null) {
            mAudienceListPopupDialog = new AudienceListPopupDialog(mContext, mAudienceListState);
        }
        mAudienceListPopupDialog.show();
    }

    private void onAudienceCountChange(int userCount) {
        setUserCount(userCount);
    }

    private void onAudienceListChange(LinkedHashSet<TUIRoomDefine.UserInfo> userInfo) {
        mAdapter.updateData();
        setUserCount(mAudienceListState.audienceCount.get());
    }

    @SuppressLint("SetTextI18n")
    private void setUserCount(int count) {
        if (mAudienceListState.audienceList.get().size() > ROOM_MAX_SHOW_USER_COUNT) {
            mTextAudienceCount.setText("" + count);
        } else {
            mTextAudienceCount.setText("" + mAudienceListState.audienceList.get().size());
        }
    }
}