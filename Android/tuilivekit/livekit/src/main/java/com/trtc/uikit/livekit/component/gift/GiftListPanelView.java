package com.trtc.uikit.livekit.component.gift;

import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.util.Log;
import android.view.View;
import android.widget.LinearLayout;

import androidx.viewpager.widget.ViewPager;

import com.trtc.uikit.livekit.common.DataReporter;
import com.trtc.uikit.livekit.component.gift.service.GiftConstants;
import com.trtc.uikit.livekit.component.gift.store.GiftStore;
import com.trtc.uikit.livekit.component.gift.store.model.Gift;
import com.trtc.uikit.livekit.component.gift.store.model.GiftUser;
import com.trtc.uikit.livekit.component.gift.view.GiftViewPagerManager;
import com.trtc.uikit.livekit.component.gift.view.IGiftListPanelView;
import com.trtc.uikit.livekit.component.gift.view.adapter.GiftViewPagerAdapter;

import java.util.ArrayList;
import java.util.List;

public class GiftListPanelView extends ViewPager implements IGiftListPanelView {
    private static final String TAG     = "GiftPanelView";
    private static final int    COLUMNS = 4;
    private static final int    ROWS    = 2;

    private GiftViewPagerManager mGiftViewManager;
    private List<View>           mGiftViewList;
    private Context              mContext;
    private String               mRoomId;
    private OnSendGiftListener   mOnSendGiftListener;

    public GiftListPanelView(Context context) {
        super(context);
    }

    public GiftListPanelView(Context context, AttributeSet attrs) {
        super(context, attrs);
        mContext = context;
        mGiftViewList = new ArrayList<>();
    }

    public void setGiftList(List<Gift> giftList) {
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
        GiftStore.sharedInstance().init(groupId);
    }

    @Override
    public void sendGift(Gift gift, int giftCount, GiftUser receiver) {
        GiftStore.sharedInstance().mGiftIMService.sendGroupGiftMessage(mRoomId, gift, receiver, giftCount);
        if (!TextUtils.isEmpty(gift.animationUrl)) {
            boolean isSvgGift = gift.animationUrl.toLowerCase().endsWith(".svga");
            int key = getReportKey(isSvgGift);
            DataReporter.reportEventData(key);
        }
    }

    public void setListener(OnSendGiftListener listener) {
        mOnSendGiftListener = listener;
    }

    public interface OnSendGiftListener {
        void onSendGift(GiftListPanelView view, Gift gift, int giftCount);
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
