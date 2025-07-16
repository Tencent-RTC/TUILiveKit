package com.trtc.uikit.livekit.component.gift;

import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.util.Log;
import android.view.View;
import android.widget.LinearLayout;

import androidx.lifecycle.Observer;
import androidx.viewpager.widget.ViewPager;

import com.tencent.cloud.tuikit.engine.extension.TUILiveGiftManager.GiftInfo;
import com.trtc.uikit.livekit.common.DataReporter;
import com.trtc.uikit.livekit.component.gift.service.GiftConstants;
import com.trtc.uikit.livekit.component.gift.service.GiftService;
import com.trtc.uikit.livekit.component.gift.store.TUIGiftStore;
import com.trtc.uikit.livekit.component.gift.view.GiftViewPagerManager;
import com.trtc.uikit.livekit.component.gift.view.adapter.GiftViewPagerAdapter;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class GiftListPanelView extends ViewPager {
    private static final String TAG     = "GiftListPanelView";
    private static final int    COLUMNS = 4;
    private static final int    ROWS    = 2;

    private       GiftViewPagerManager mGiftViewManager;
    private       List<View>           mGiftViewList;
    private       Context              mContext;
    private       String               mRoomId;
    private       OnSendGiftListener   mOnSendGiftListener;
    private final GiftService          mGiftService = new GiftService();

    private final Observer<Map<String, List<GiftInfo>>> mGiftListObserver = this::onGiftListChange;

    public GiftListPanelView(Context context) {
        super(context);
    }

    public GiftListPanelView(Context context, AttributeSet attrs) {
        super(context, attrs);
        mContext = context;
        mGiftViewList = new ArrayList<>();
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        mGiftService.updateGiftList();
        TUIGiftStore.sharedInstance().mGiftListMap.observeForever(mGiftListObserver);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        mGiftService.unInit();
        TUIGiftStore.sharedInstance().mGiftListMap.removeObserver(mGiftListObserver);
    }

    private void setGiftList(List<GiftInfo> giftList) {
        if (giftList == null || giftList.isEmpty()) {
            Log.w(TAG, "giftList empty!");
            return;
        }
        if (mGiftViewManager == null) {
            mGiftViewManager = new GiftViewPagerManager();
            mGiftViewManager.setGiftClickListener((position, giftModel) -> {
                if (mOnSendGiftListener != null) {
                    mOnSendGiftListener.onSendGift(this, giftModel, 1);
                }
            });
        }
        if (mGiftViewList != null && !mGiftViewList.isEmpty()) {
            mGiftViewList.clear();
        }
        int pageSize = mGiftViewManager.getPagerCount(giftList.size(), COLUMNS, ROWS);
        // Count the number of pages
        for (int i = 0; i < pageSize; i++) {
            mGiftViewList.add(mGiftViewManager.viewPagerItem(mContext, i, giftList, COLUMNS, ROWS));
            LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(16, 16);
            params.setMargins(10, 0, 10, 0);
        }
        GiftViewPagerAdapter mGiftViewPagerAdapter = new GiftViewPagerAdapter(mGiftViewList);
        this.setAdapter(mGiftViewPagerAdapter);
        this.setCurrentItem(0);
    }

    public void init(String groupId) {
        mRoomId = groupId;
        mGiftService.init(groupId, null);
    }

    public void sendGift(GiftInfo gift, int giftCount) {
        mGiftService.sendGift(gift, giftCount);
        if (!TextUtils.isEmpty(gift.resourceUrl)) {
            boolean isSvgGift = gift.resourceUrl.toLowerCase().endsWith(".svga");
            int key = getReportKey(isSvgGift);
            DataReporter.reportEventData(key);
        }
    }

    public void setListener(OnSendGiftListener listener) {
        mOnSendGiftListener = listener;
    }

    public interface OnSendGiftListener {
        void onSendGift(GiftListPanelView view, GiftInfo giftInfo, int giftCount);
    }

    private void onGiftListChange(Map<String, List<GiftInfo>> map) {
        if (!TextUtils.isEmpty(mRoomId)) {
            setGiftList(map.get(mRoomId));
        }
    }

    private int getReportKey(boolean isSvgGift) {
        boolean isVoiceRoom = !TextUtils.isEmpty(mRoomId) && mRoomId.startsWith("voice_");
        int key;
        if (isVoiceRoom) {
            key = isSvgGift
                    ? GiftConstants.DATA_REPORT_VOICE_GIFT_SVGA_SEND_COUNT
                    : GiftConstants.DATA_REPORT_VOICE_GIFT_EFFECT_SEND_COUNT;
        } else {
            key = isSvgGift
                    ? GiftConstants.DATA_REPORT_LIVE_GIFT_SVGA_SEND_COUNT
                    : GiftConstants.DATA_REPORT_LIVE_GIFT_EFFECT_SEND_COUNT;
        }
        return key;
    }
}
