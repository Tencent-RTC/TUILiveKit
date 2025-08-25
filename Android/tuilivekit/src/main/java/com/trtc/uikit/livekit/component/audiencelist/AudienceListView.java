package com.trtc.uikit.livekit.component.audiencelist;

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

import androidx.lifecycle.Observer;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.DataReporter;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.audiencelist.service.AudienceListObserver;
import com.trtc.uikit.livekit.component.audiencelist.service.AudienceListService;
import com.trtc.uikit.livekit.component.audiencelist.store.AudienceListState;
import com.trtc.uikit.livekit.component.audiencelist.view.AudienceListPopupDialog;
import com.trtc.uikit.livekit.component.audiencelist.view.adapter.AudienceListIconAdapter;

import java.util.Locale;
import java.util.Set;

@SuppressLint("ViewConstructor")
public class AudienceListView extends FrameLayout {
    private static final int MAX_SHOW_AVATAR_COUNT                               = 2;
    private static final int LIVEKIT_METRICS_PANEL_SHOW_LIVE_ROOM_AUDIENCE_LIST  = 190010;
    private static final int LIVEKIT_METRICS_PANEL_SHOW_VOICE_ROOM_AUDIENCE_LIST = 191009;

    private final Context                               mContext;
    private       RecyclerView                          mRecycleAudienceList;
    private       AudienceListIconAdapter               mAdapter;
    private       TextView                              mTextAudienceCount;
    private       LinearLayout                          mLayoutAudienceCount;
    private       AudienceListObserver                  mAudienceObserver;
    private final AudienceListService                   mAudienceListService
            = new AudienceListService();
    private final AudienceListState                     mAudienceListState
            = mAudienceListService.mAudienceListState;
    private final Observer<Set<TUIRoomDefine.UserInfo>> mAudienceListObserver
            = this::onAudienceListChange;
    private final Observer<Integer>                     mAudienceCountObserver
            = this::onAudienceCountChange;
    private final Observer<Boolean>                     mRoomDismissedObserver
            = this::onRoomDismissed;
    private       TUIRoomEngine                         mRoomEngine;
    private       AudienceListPopupDialog               mAudienceListPopupDialog;
    private       OnUserItemClickListener               mOnUserItemClickListener;

    private static final LiveKitLogger LOGGER = LiveKitLogger.getComponentLogger("AudienceListView");

    public AudienceListView(Context context) {
        this(context, null);
    }

    public AudienceListView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AudienceListView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        LayoutInflater.from(mContext).inflate(R.layout.audience_list_layout_icon, this, true);
    }

    @Deprecated
    public void init(TUIRoomDefine.RoomInfo roomInfo) {
        init(convertToLiveInfo(roomInfo));
    }

    public void init(TUILiveListManager.LiveInfo liveInfo) {
        mAudienceListService.initRoomInfo(liveInfo);
        reportData(liveInfo.roomId);
    }

    public void setOnUserItemClickListener(OnUserItemClickListener listener) {
        mOnUserItemClickListener = listener;
        if (mAudienceListPopupDialog != null) {
            mAudienceListPopupDialog.setOnUserItemClickListener(listener);
        }
    }

    public void setScreenOrientation(boolean isPortrait) {
        if (mLayoutAudienceCount != null) {
            mLayoutAudienceCount.setEnabled(isPortrait);
        }
        if (mRecycleAudienceList != null) {
            mRecycleAudienceList.setEnabled(isPortrait);
        }
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

    private TUILiveListManager.LiveInfo convertToLiveInfo(TUIRoomDefine.RoomInfo roomInfo) {
        TUILiveListManager.LiveInfo liveInfo = new TUILiveListManager.LiveInfo();
        liveInfo.roomId = roomInfo.roomId;
        liveInfo.name = roomInfo.name;
        liveInfo.ownerId = roomInfo.ownerId;
        liveInfo.ownerName = roomInfo.ownerName;
        liveInfo.ownerAvatarUrl = roomInfo.ownerAvatarUrl;
        return liveInfo;
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
        mAudienceListState.audienceCount.observeForever(mAudienceCountObserver);
        mAudienceListState.audienceList.observeForever(mAudienceListObserver);
        mAudienceListState.isRoomDismissed.observeForever(mRoomDismissedObserver);
    }

    protected void removeObserver() {
        mAudienceListState.audienceCount.removeObserver(mAudienceCountObserver);
        mAudienceListState.audienceList.removeObserver(mAudienceListObserver);
        mAudienceListState.isRoomDismissed.removeObserver(mRoomDismissedObserver);
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
        mAudienceListService.getAudienceList();
        if (mAudienceListPopupDialog == null) {
            mAudienceListPopupDialog = new AudienceListPopupDialog(mContext, mAudienceListState);
            mAudienceListPopupDialog.setOnUserItemClickListener(mOnUserItemClickListener);
        }
        mAudienceListPopupDialog.show();
    }

    private void onRoomDismissed(Boolean isDismissed) {
        if (Boolean.TRUE.equals(isDismissed)) {
            if (mAudienceListPopupDialog != null) {
                mAudienceListPopupDialog.dismiss();
            }
        }
    }

    private void onAudienceCountChange(int userCount) {
        updateAudienceCount();
    }

    private void onAudienceListChange(Set<TUIRoomDefine.UserInfo> userInfo) {
        RelativeLayout.LayoutParams params = (RelativeLayout.LayoutParams) mRecycleAudienceList.getLayoutParams();
        if (userInfo.size() <= MAX_SHOW_AVATAR_COUNT) {
            params.width = RelativeLayout.LayoutParams.WRAP_CONTENT;
        } else {
            params.width = ScreenUtil.dip2px(56);
        }
        mAdapter.updateData();
        updateAudienceCount();
    }

    private void updateAudienceCount() {
        int userCount = mAudienceListState.audienceCount.getValue();
        int listSize = mAudienceListState.audienceList.getValue().size();
        LOGGER.info("updateAudienceCount userCount:" + userCount + " listSize:" + listSize);
        if (userCount >= AudienceListState.ROOM_MAX_SHOW_USER_COUNT) {
            setUserCount(userCount);
        } else {
            setUserCount(listSize);
        }
    }

    @SuppressLint("SetTextI18n")
    private void setUserCount(int count) {
        String text = formatUserCount(count);
        mTextAudienceCount.setText(text);
    }

    private String formatUserCount(int count) {
        String text = "" + count;
        if (count >= 10000) {
            boolean isChinese = Locale.CHINESE.getLanguage().equals(Locale.getDefault().getLanguage());
            if (isChinese) {
                text = String.format(Locale.getDefault(), "%.1f", count / 10000.0F);
                if (text.endsWith(".0")) {
                    text = text.replace(".0", "");
                }
                text = text + "万";
            } else {
                text = String.format(Locale.getDefault(), "%dk", count / 1000);
            }
        }
        return text;
    }

    private void reportData(String roomId) {
        boolean isVoiceRoom = !TextUtils.isEmpty(roomId) && roomId.startsWith("voice_");
        if (isVoiceRoom) {
            DataReporter.reportEventData(LIVEKIT_METRICS_PANEL_SHOW_VOICE_ROOM_AUDIENCE_LIST);
        } else {
            DataReporter.reportEventData(LIVEKIT_METRICS_PANEL_SHOW_LIVE_ROOM_AUDIENCE_LIST);
        }
    }

    public interface OnUserItemClickListener {
        void onUserItemClick(TUIRoomDefine.UserInfo userInfo);
    }
}