package com.trtc.uikit.livekit.common.uicomponent.barrage;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Color;
import android.text.Spannable;
import android.text.SpannableStringBuilder;
import android.text.TextUtils;
import android.text.style.ForegroundColorSpan;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.TextView;

import androidx.core.util.Pair;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.barrage.model.TUIBarrage;
import com.trtc.uikit.livekit.common.uicomponent.barrage.service.BarrageIMService;
import com.trtc.uikit.livekit.common.uicomponent.barrage.service.BarragePresenter;
import com.trtc.uikit.livekit.common.uicomponent.barrage.service.IBarrageMessage;
import com.trtc.uikit.livekit.common.uicomponent.barrage.service.IBarragePresenter;
import com.trtc.uikit.livekit.common.uicomponent.barrage.store.BarrageStore;
import com.trtc.uikit.livekit.common.uicomponent.barrage.view.IBarrageDisplayView;
import com.trtc.uikit.livekit.common.uicomponent.barrage.view.adapter.BarrageMsgListAdapter;
import com.trtc.uikit.livekit.common.uicomponent.barrage.view.adapter.TUIBarrageDisplayAdapter;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;

import java.util.ArrayList;

@SuppressLint("ViewConstructor")
public class TUIBarrageDisplayView extends FrameLayout implements IBarrageDisplayView {
    private static final String TAG = "TUIBarrageDisplayView";

    private TextView              mTextNotice;
    private RecyclerView          mRecyclerMsg;
    private BarrageMsgListAdapter mAdapter;
    private ArrayList<TUIBarrage> mMsgList;
    private int                   mBarrageCount = 0;
    private String                mRoomId;
    private String                mOwnerId;

    private final IBarragePresenter mPresenter;
    private final Observer<Pair<String, TUIBarrage>> mBarrageObserver = barragePair -> {
        if (barragePair != null && TextUtils.equals(mRoomId, barragePair.first)) {
            insertBarrages(barragePair.second);
        }
    };

    public TUIBarrageDisplayView(Context context, String roomId, String ownerId) {
        this(context, new BarrageIMService(roomId));
        mRoomId = roomId;
        mOwnerId = ownerId;
    }

    private TUIBarrageDisplayView(Context context, IBarrageMessage service) {
        super(context);
        mPresenter = new BarragePresenter(context, service);
        initView(context);
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        mPresenter.initDisplayView(this);
        addObserver();
        BarrageStore.sharedInstance().mSendBarrage.set(null);
    }

    private void addObserver() {
        BarrageStore.sharedInstance().mSendBarrage.observe(mBarrageObserver);
    }

    private void removeObserver() {
        BarrageStore.sharedInstance().mSendBarrage.removeObserver(mBarrageObserver);
    }

    private void initView(Context context) {
        LayoutInflater.from(context).inflate(R.layout.livekit_barrage_view_display, this);
        mTextNotice = findViewById(R.id.tv_notice);
        mRecyclerMsg = findViewById(R.id.rv_msg);
        mMsgList = new ArrayList<>();
        mAdapter = new BarrageMsgListAdapter(context, mOwnerId, mMsgList, null);
        mRecyclerMsg.setLayoutManager(new LinearLayoutManager(context));
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

    public void setAdapter(TUIBarrageDisplayAdapter adapter) {
        mAdapter.setCustomAdapter(adapter);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        mPresenter.destroyPresenter();
        removeObserver();
        BarrageStore.sharedInstance().mSendBarrage.set(null);
    }

    @SuppressLint("NotifyDataSetChanged")
    @Override
    public void insertBarrages(TUIBarrage... barrages) {
        if (barrages == null) {
            return;
        }
        for (TUIBarrage barrage : barrages) {
            if (barrage == null) {
                LiveKitLog.error(TAG + " insertBarrage barrages is empty");
                continue;
            }
            String message = barrage.content;
            LiveKitLog.info(TAG + " insertBarrage message = " + message);
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
