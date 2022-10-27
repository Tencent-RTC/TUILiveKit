package com.tencent.qcloud.tuikit.tuigift.view;


import android.content.Context;
import android.view.LayoutInflater;

import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.tencent.qcloud.tuikit.tuigift.R;
import com.tencent.qcloud.tuikit.tuigift.core.TUIGiftExtension;
import com.tencent.qcloud.tuikit.tuigift.model.TUIGiftModel;
import com.tencent.qcloud.tuikit.tuigift.presenter.TUIGiftPresenter;

import java.lang.ref.WeakReference;


/**
 * 礼物面板
 */
public class TUIGiftListPanelPlugView extends BottomSheetDialog {
    private Context              mContext;
    private LayoutInflater       mLayoutInflater;
    private TUIGiftListPanelView mPanelView;
    private TUIGiftPresenter     mPresenter;
    private String               mGroupId;

    public TUIGiftListPanelPlugView(Context context, String groupId) {
        super(context, R.style.TUIGiftListPanelViewTheme);
        mContext = context;
        setContentView(R.layout.tuigift_panel);
        this.mGroupId = groupId;
        init();
    }

    private void init() {
        mLayoutInflater = LayoutInflater.from(mContext);
        mPanelView = findViewById(R.id.gift_panel_view_pager);
        mPanelView.init(mGroupId);
        setCanceledOnTouchOutside(true);
        TUIGiftExtension.map.put(mGroupId + TUIGiftExtension.KEY_TYPE_PANEL, new WeakReference<Object>(mPanelView));
        TUIGiftExtension.map.put(mGroupId + TUIGiftExtension.KEY_TYPE_PLUG, new WeakReference<Object>(this));
    }

    public void show() {
        super.show();
        initGiftData();
    }

    /**
     * 初始化礼物面板
     */
    public void initGiftData() {
        if (mPresenter == null) {
            mPresenter = mPanelView.getPresenter();
        }
        mPresenter.initGiftData();
    }

    /**
     * 设置Listener处理发送礼物事件
     */
    public void setListener() {
        mPanelView.setListener(new TUIGiftListener() {
            @Override
            public void onSendGiftSuccess(int code, String msg, TUIGiftModel giftModel) {
                if (TUIGiftExtension.map.get(mGroupId + TUIGiftExtension.KEY_TYPE_PLAY) != null) {
                    ((TUIGiftPlayView) TUIGiftExtension.map.get(mGroupId + TUIGiftExtension.KEY_TYPE_PLAY).get())
                            .receiveGift(giftModel);
                }
            }

            @Override
            public void onSendLikeSuccess(int code, String msg) {
                if (TUIGiftExtension.map.get(mGroupId + TUIGiftExtension.KEY_TYPE_PLAY) != null) {
                    ((TUIGiftPlayView) TUIGiftExtension.map.get(mGroupId + TUIGiftExtension.KEY_TYPE_PLAY).get())
                            .receiveLike();
                }
            }

            @Override
            public void onFailed(int code, String msg) {

            }
        });

    }
}
