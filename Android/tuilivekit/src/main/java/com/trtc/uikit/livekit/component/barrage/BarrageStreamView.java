package com.trtc.uikit.livekit.component.barrage;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Color;
import android.text.Spannable;
import android.text.SpannableStringBuilder;
import android.text.TextUtils;
import android.text.style.ForegroundColorSpan;
import android.util.AttributeSet;
import android.util.Log;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.TextView;

import androidx.core.util.Pair;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.barrage.service.BarrageIMService;
import com.trtc.uikit.livekit.component.barrage.service.BarragePresenter;
import com.trtc.uikit.livekit.component.barrage.service.IBarragePresenter;
import com.trtc.uikit.livekit.component.barrage.store.BarrageStore;
import com.trtc.uikit.livekit.component.barrage.store.model.Barrage;
import com.trtc.uikit.livekit.component.barrage.view.IBarrageDisplayView;
import com.trtc.uikit.livekit.component.barrage.view.adapter.BarrageItemAdapter;
import com.trtc.uikit.livekit.component.barrage.view.adapter.BarrageItemDefaultAdapter;
import com.trtc.uikit.livekit.component.barrage.view.adapter.BarrageItemTypeDelegate;
import com.trtc.uikit.livekit.component.barrage.view.adapter.BarrageMsgListAdapter;

import java.util.ArrayList;

@SuppressLint("ViewConstructor")
public class BarrageStreamView extends FrameLayout implements IBarrageDisplayView {
    private static final String                TAG           = "BarrageStreamView";
    private final        Context               mContext;
    private              TextView              mTextNotice;
    private              RecyclerView          mRecyclerMsg;
    private              BarrageMsgListAdapter mAdapter;
    private              ArrayList<Barrage>    mMsgList;
    private              int                   mBarrageCount = 0;
    private              String                mRoomId;
    private final        BarrageIMService      mService;
    private final        IBarragePresenter     mPresenter;

    private final Observer<Pair<String, Barrage>> mBarrageObserver = barragePair -> {
        if (barragePair != null && TextUtils.equals(mRoomId, barragePair.first)) {
            insertBarrages(barragePair.second);
        }
    };

    public BarrageStreamView(Context context) {
        this(context, null);
    }

    public BarrageStreamView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public BarrageStreamView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        mService = new BarrageIMService("");
        mPresenter = new BarragePresenter(mContext, mService);
        initView();
    }

    public void init(String roomId, String ownerId) {
        mRoomId = roomId;
        mService.setRoomId(roomId);
        mPresenter.initDisplayView(this);
        mAdapter.setItemAdapter(0, new BarrageItemDefaultAdapter(mContext, ownerId));
    }

    public void setItemTypeDelegate(BarrageItemTypeDelegate delegate) {
        mAdapter.setItemTypeDelegate(delegate);
    }

    public void setItemAdapter(int itemType, BarrageItemAdapter adapter) {
        mAdapter.setItemAdapter(itemType, adapter);
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        addObserver();
        BarrageStore.sharedInstance().mSendBarrage.set(null);
    }

    private void addObserver() {
        BarrageStore.sharedInstance().mSendBarrage.observe(mBarrageObserver);
    }

    private void removeObserver() {
        BarrageStore.sharedInstance().mSendBarrage.removeObserver(mBarrageObserver);
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
        if (mPresenter != null) {
            mPresenter.destroyPresenter();
        }
        removeObserver();
        BarrageStore.sharedInstance().mSendBarrage.set(null);
    }

    @SuppressLint("NotifyDataSetChanged")
    @Override
    public void insertBarrages(Barrage... barrages) {
        if (barrages == null) {
            return;
        }
        for (Barrage barrage : barrages) {
            if (barrage == null) {
                Log.i(TAG, "insertBarrage barrages is empty");
                continue;
            }
            String message = barrage.content;
            Log.i(TAG, "insertBarrage message = " + message);
            mMsgList.add(barrage);
            mBarrageCount++;
        }
        mAdapter.notifyDataSetChanged();
        mRecyclerMsg.smoothScrollToPosition(mAdapter.getItemCount());
    }

    public int getBarrageCount() {
        return mBarrageCount;
    }
}
