package com.trtc.uikit.component.audiencelist;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Point;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.component.audiencelist.service.AudienceListObserver;
import com.trtc.uikit.component.audiencelist.service.AudienceListService;
import com.trtc.uikit.component.audiencelist.store.AudienceListState;
import com.trtc.uikit.component.audiencelist.view.AudienceListPopupDialog;
import com.trtc.uikit.component.audiencelist.view.adapter.AudienceListIconAdapter;
import com.trtc.uikit.component.common.DataReporter;

import java.util.LinkedHashSet;

@SuppressLint("ViewConstructor")
public class AudienceListView extends FrameLayout {
    private static final int MAX_SHOW_AVATAR_COUNT                               = 3;
    private static final int LIVEKIT_METRICS_PANEL_SHOW_LIVE_ROOM_AUDIENCE_LIST  = 190010;
    private static final int LIVEKIT_METRICS_PANEL_SHOW_VOICE_ROOM_AUDIENCE_LIST = 191009;

    private final Context                                         mContext;
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
        reportData(roomId);
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
        mAudienceObserver = new AudienceListObserver(mAudienceListService);
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
            private final Point   point  = new Point();
            private       boolean scroll = false;

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
        RelativeLayout.LayoutParams params = (RelativeLayout.LayoutParams) mRecycleAudienceList.getLayoutParams();
        if (userInfo.size() <= MAX_SHOW_AVATAR_COUNT) {
            params.width = RelativeLayout.LayoutParams.WRAP_CONTENT;
        } else {
            params.width = ScreenUtil.dip2px(78);
        }
        mAdapter.updateData();
        setUserCount(mAudienceListState.audienceCount.get());
    }

    @SuppressLint("SetTextI18n")
    private void setUserCount(int count) {
        if (mAudienceListState.audienceCount.get() > AudienceListState.ROOM_MAX_SHOW_USER_COUNT) {
            mTextAudienceCount.setText("" + count);
        } else {
            mTextAudienceCount.setText("" + mAudienceListState.audienceList.get().size());
        }
    }

    private void reportData(String roomId) {
        boolean isVoiceRoom = !TextUtils.isEmpty(roomId) && roomId.startsWith("voice_");
        if (isVoiceRoom) {
            DataReporter.reportEventData(LIVEKIT_METRICS_PANEL_SHOW_VOICE_ROOM_AUDIENCE_LIST);
        } else {
            DataReporter.reportEventData(LIVEKIT_METRICS_PANEL_SHOW_LIVE_ROOM_AUDIENCE_LIST);
        }
    }
}