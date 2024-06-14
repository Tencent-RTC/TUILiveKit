package com.trtc.uikit.livekit.common.uicomponent.gift;

import android.content.Context;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGift;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGiftUser;
import com.trtc.uikit.livekit.common.uicomponent.gift.view.GiftListPanelView;

import java.util.List;

public class TUIGiftListView extends BottomSheetDialog {
    public static final String TAG = "TUIGiftListView";

    private       GiftListPanelView mPanelView;
    private final String            mRoomId;

    private TextView          mBalanceView;

    private OnGiftListener    mOnGiftListener;


    public TUIGiftListView(Context context, String roomId) {
        super(context);
        setContentView(R.layout.livekit_gift_panel);
        this.mRoomId = roomId;
        init();
    }

    private void init() {
        mPanelView = findViewById(R.id.gift_panel_view_pager);
        mPanelView.init(mRoomId);
        mPanelView.setListener((giftModel, giftCount) -> {
            if (mOnGiftListener != null) {
                mOnGiftListener.onSendGift(TUIGiftListView.this, giftModel, giftCount);
            }
        });
        mBalanceView = findViewById(R.id.tv_balance);
        Button rechargeView = findViewById(R.id.btn_recharge);
        rechargeView.setOnClickListener(v -> {
            if (mOnGiftListener != null) {
                mOnGiftListener.onRecharge(TUIGiftListView.this);
            }
        });
        setCanceledOnTouchOutside(true);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        View bottomSheet = findViewById(com.google.android.material.R.id.design_bottom_sheet);
        bottomSheet.setBackgroundResource(com.trtc.tuikit.common.R.color.common_design_bottom_sheet_color);
    }

    public void setBalance(int balance) {
        mBalanceView.setText(String.valueOf(balance));
    }

    public void setGiftList(List<TUIGift> list) {
        mPanelView.setGiftModelSource(list);
    }

    public void setListener(OnGiftListener listener) {
        mOnGiftListener = listener;
    }

    public void sendGift(TUIGift gift, int giftCount, TUIGiftUser receiver) {
        mPanelView.sendGift(gift, giftCount, receiver);
    }

    public interface OnGiftListener {
        void onRecharge(TUIGiftListView view);

        void onSendGift(TUIGiftListView view, TUIGift gift, int giftCount);
    }
}
