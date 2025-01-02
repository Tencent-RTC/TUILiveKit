package com.trtc.uikit.component.barrage;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Color;
import android.text.Spannable;
import android.text.SpannableStringBuilder;
import android.text.TextUtils;
import android.text.style.ForegroundColorSpan;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.TextView;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.component.barrage.service.BarrageConstants;
import com.trtc.uikit.component.barrage.service.BarrageIMService;
import com.trtc.uikit.component.barrage.store.BarrageState;
import com.trtc.uikit.component.barrage.store.BarrageStore;
import com.trtc.uikit.component.barrage.store.model.Barrage;
import com.trtc.uikit.component.barrage.view.IBarrageDisplayView;
import com.trtc.uikit.component.barrage.view.adapter.BarrageItemAdapter;
import com.trtc.uikit.component.barrage.view.adapter.BarrageItemDefaultAdapter;
import com.trtc.uikit.component.barrage.view.adapter.BarrageItemTypeDelegate;
import com.trtc.uikit.component.barrage.view.adapter.BarrageMsgListAdapter;
import com.trtc.uikit.component.common.DataReporter;

import java.util.ArrayList;
import java.util.List;

@SuppressLint("ViewConstructor")
public class BarrageStreamView extends FrameLayout implements IBarrageDisplayView {
    private static final String TAG                             = "BarrageStreamView";
    private static final int    BARRAGE_LIST_UPDATE_DURATION_MS = 250;

    private final Context               mContext;
    private       TextView              mTextNotice;
    private       RecyclerView          mRecyclerMsg;
    private       BarrageMsgListAdapter mAdapter;
    private       ArrayList<Barrage>    mMsgList;
    private       String                mRoomId;
    private final BarrageIMService      mService;
    private       long                  mTimestampOnLastUpdate = 0;
    private       BarrageState          mBarrageState;

    private final Runnable                mUpdateViewTask      = this::notifyDataSetChanged;
    private final Observer<List<Barrage>> mBarrageListObserver = this::onBarrageListChanged;

    public BarrageStreamView(Context context) {
        this(context, null);
    }

    public BarrageStreamView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public BarrageStreamView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        mService = BarrageStore.sharedInstance().mBarrageIMService;
        initView();
    }

    public void init(String roomId, String ownerId) {
        mRoomId = roomId;
        mAdapter.setItemAdapter(0, new BarrageItemDefaultAdapter(mContext, ownerId));
        initState();
        reportData();
    }

    public void setItemTypeDelegate(BarrageItemTypeDelegate delegate) {
        mAdapter.setItemTypeDelegate(delegate);
    }

    public void setItemAdapter(int itemType, BarrageItemAdapter adapter) {
        mAdapter.setItemAdapter(itemType, adapter);
    }

    public void setMaxDataCount(int count) {
        mService.setMaxBarrageCount(count);
    }

    private void initState() {
        if (mBarrageState != null) {
            return;
        }
        mBarrageState = BarrageStore.sharedInstance().getBarrageState(mRoomId);
        addObserver();
    }

    private void addObserver() {
        mBarrageState.mBarrageCacheList.observe(mBarrageListObserver);
    }

    private void removeObserver() {
        if (mBarrageState != null) {
            mBarrageState.mBarrageCacheList.removeObserver(mBarrageListObserver);
        }
    }

    private void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_barrage_view_display, this);
        mTextNotice = findViewById(R.id.tv_notice);
        mRecyclerMsg = findViewById(R.id.rv_msg);
        mMsgList = new ArrayList<>();
        mRecyclerMsg.setLayoutManager(new LinearLayoutManager(mContext));
        mAdapter = new BarrageMsgListAdapter(mMsgList);
        mRecyclerMsg.setAdapter(mAdapter);
    }

    private void setNotice(String username, String notice) {
        username = TextUtils.isEmpty(username) ? "" : username;
        notice = TextUtils.isEmpty(notice) ? "" : notice;
        String result = username + notice;
        SpannableStringBuilder builder = new SpannableStringBuilder(result);
        ForegroundColorSpan redSpan = new ForegroundColorSpan(Color.BLUE);
        builder.setSpan(redSpan, 0, username.length(), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
        mTextNotice.setText(builder);
        mTextNotice.setBackgroundResource(R.drawable.livekit_barrage_bg_msg_item);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    @Override
    public void insertBarrages(Barrage... barrages) {
        mService.insertBarrages(mRoomId, barrages);
    }

    public void onBarrageListChanged(List<Barrage> barrages) {
        if (mMsgList == null) {
            return;
        }
        mMsgList.clear();
        mMsgList.addAll(barrages);
        removeCallbacks(mUpdateViewTask);
        if (System.currentTimeMillis() - mTimestampOnLastUpdate >= BARRAGE_LIST_UPDATE_DURATION_MS) {
            notifyDataSetChanged();
        } else {
            postDelayed(mUpdateViewTask, BARRAGE_LIST_UPDATE_DURATION_MS);
        }
    }

    public int getBarrageCount() {
        return mBarrageState == null ? 0 : mBarrageState.mBarrageTotalCount.get();
    }

    @SuppressLint("NotifyDataSetChanged")
    private void notifyDataSetChanged() {
        mTimestampOnLastUpdate = System.currentTimeMillis();
        mAdapter.notifyDataSetChanged();
        mRecyclerMsg.smoothScrollToPosition(mAdapter.getItemCount());
    }

    private void reportData() {
        boolean isVoiceRoom = !TextUtils.isEmpty(mRoomId) && mRoomId.startsWith("voice_");
        int key = BarrageConstants.LIVEKIT_METRICS_PANEL_SHOW_LIVE_ROOM_BARRAGE_SHOW;
        if (isVoiceRoom) {
            key = BarrageConstants.LIVEKIT_METRICS_PANEL_SHOW_VOICE_ROOM_BARRAGE_SHOW;
        }
        DataReporter.reportEventData(key);
    }
}
