package com.tencent.qcloud.tuikit.tuigift.view;

import android.content.Context;
import android.util.AttributeSet;
import android.util.Log;
import android.view.View;
import android.widget.LinearLayout;

import androidx.viewpager.widget.ViewPager;

import com.tencent.qcloud.tuikit.tuigift.model.TUIGiftModel;
import com.tencent.qcloud.tuikit.tuigift.presenter.TUIGiftCallBack;
import com.tencent.qcloud.tuikit.tuigift.presenter.TUIGiftPresenter;
import com.tencent.qcloud.tuikit.tuigift.view.adapter.TUIGiftViewPagerAdapter;

import java.util.ArrayList;
import java.util.List;

/**
 * 礼物面板内的viewpager
 */
public class TUIGiftListPanelView extends ViewPager implements ITUIGiftListPanelView {
    private static final String TAG     = "TUIGiftPanelView";
    private static final int    COLUMNS = 5;
    private static final int    ROWS    = 1;

    private TUIGiftViewPagerManager mGiftViewManager;
    private TUIGiftViewPagerAdapter mGiftViewPagerAdapter;
    private List<View>              mGiftViewList;
    private List<TUIGiftModel>      mGiftModelSource;
    private TUIGiftListener         mListener;
    private Context                 mContext;
    private TUIGiftPresenter        mPresenter;
    private String                  mGroupId;
    private long                    mSendLikeDate;
    private int                     mCurrentLikeCount;

    private static final int mMaxLikeCount = 20;
    private static final int mMinDuration  = 5;

    public TUIGiftListPanelView(Context context) {
        super(context);
    }

    public TUIGiftListPanelView(Context context, AttributeSet attrs) {
        super(context, attrs);
        mContext = context;
        mGiftViewList = new ArrayList<>();
    }

    /**
     * 初始化Presenter
     */
    private void initPresenter() {
        mPresenter = new TUIGiftPresenter(mContext, mGroupId);
        mPresenter.initGiftPanelView(this);
    }

    /**
     * 设置礼物数据源
     *
     * @param giftDataSource 礼物数据源
     */
    public void setGiftModelSource(List<TUIGiftModel> giftDataSource) {
        mGiftModelSource = giftDataSource;
    }

    /**
     * 初始化view
     *
     * @param groupId 传入的groupId
     */
    public void init(String groupId) {
        mGroupId = groupId;
        initPresenter();
        if (mGiftModelSource == null || mGiftModelSource.size() == 0) {
            Log.i(TAG, "giftModelSource empty!");
            return;
        }
        if (mGiftViewManager == null) {
            mGiftViewManager = new TUIGiftViewPagerManager();
        }
        mGiftViewManager.setGiftClickListener(new TUIGiftViewPagerManager.GiftClickListener() {
            @Override
            public void onClick(int position, TUIGiftModel giftModel) {
                if (mGiftViewManager == null) {
                    return;
                }
                if (giftModel != null) {
                    Log.d(TAG, "onGiftItemClick: " + giftModel);
                    sendGift(giftModel);
                }
            }
        });
        if (mGiftViewList != null && !mGiftViewList.isEmpty()) {
            mGiftViewList.clear();
        }
        int pageSize = mGiftViewManager.getPagerCount(mGiftModelSource.size(), COLUMNS, ROWS);
        // 获取页数
        for (int i = 0; i < pageSize; i++) {
            mGiftViewList.add(mGiftViewManager.viewPagerItem(mContext, i, mGiftModelSource, COLUMNS, ROWS));
            LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(16, 16);
            params.setMargins(10, 0, 10, 0);
        }
        mGiftViewPagerAdapter = new TUIGiftViewPagerAdapter(mGiftViewList);
        this.setAdapter(mGiftViewPagerAdapter);
        this.setCurrentItem(0);
    }

    public void setListener(TUIGiftListener listener) {
        mListener = listener;
    }

    public TUIGiftPresenter getPresenter() {
        if (mPresenter == null) {
            initPresenter();
        }
        mCurrentLikeCount = 0;
        mSendLikeDate = System.currentTimeMillis() / 1000;
        return mPresenter;
    }

    /**
     * 发送礼物
     *
     * @param giftModel 待发送的礼物信息
     */
    @Override
    public void sendGift(TUIGiftModel giftModel) {
        mPresenter.sendGroupGiftMessage(giftModel, new TUIGiftCallBack.GiftSendCallBack() {
            @Override
            public void onSuccess(int code, String msg, TUIGiftModel giftModel) {
                if (mListener != null) {
                    mListener.onSendGiftSuccess(code, msg, giftModel);
                }
            }

            @Override
            public void onFailed(int code, String msg) {
                if (mListener != null) {
                    mListener.onFailed(code, msg);
                }
            }
        });
    }

    @Override
    public void sendLike() {
        if (mCurrentLikeCount >= mMaxLikeCount) {
            sendLikeByPresenter();
            mCurrentLikeCount = 0;
            mSendLikeDate = System.currentTimeMillis() / 1000;
        }
        int duration = (int) (System.currentTimeMillis() / 1000 - mSendLikeDate);
        if (duration >= mMinDuration) {
            sendLikeByPresenter();
            mCurrentLikeCount = 0;
            mSendLikeDate = System.currentTimeMillis() / 1000;
        } else {
            mCurrentLikeCount += 1;
            if (mListener != null) {
                mListener.onSendLikeSuccess(0, "send like by local");
            }
        }
    }

    private void sendLikeByPresenter() {
        mPresenter.sendGroupLikeMessage(new TUIGiftCallBack.GiftSendCallBack() {
            @Override
            public void onSuccess(int code, String msg, TUIGiftModel giftModel) {
                if (mListener != null) {
                    mListener.onSendLikeSuccess(code, msg);
                }
            }

            @Override
            public void onFailed(int code, String msg) {
                if (mListener != null) {
                    mListener.onFailed(code, msg);
                }
            }
        });
    }
}
