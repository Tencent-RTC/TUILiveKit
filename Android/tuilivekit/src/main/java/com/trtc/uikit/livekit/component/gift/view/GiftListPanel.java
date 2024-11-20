package com.trtc.uikit.livekit.component.gift.view;

import android.content.Context;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.gift.store.model.Gift;
import com.trtc.uikit.livekit.component.gift.store.model.GiftUser;

import java.util.List;

public class GiftListPanel extends BottomSheetDialog {

    public static final String            TAG = "GiftListPanel";
    private             GiftListPanelView mPanelView;
    private final       String            mRoomId;
    private             TextView          mBalanceView;
    private             OnGiftListener    mOnGiftListener;

    public GiftListPanel(Context context, String roomId) {
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
                mOnGiftListener.onSendGift(GiftListPanel.this, giftModel, giftCount);
            }
        });
        mBalanceView = findViewById(R.id.tv_balance);
        Button rechargeView = findViewById(R.id.btn_recharge);
        rechargeView.setOnClickListener(v -> {
            if (mOnGiftListener != null) {
                mOnGiftListener.onRecharge(GiftListPanel.this);
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

    public void setGiftList(List<Gift> list) {
        mPanelView.setGiftModelSource(list);
    }

    public void setListener(OnGiftListener listener) {
        mOnGiftListener = listener;
    }

    public void sendGift(Gift gift, int giftCount, GiftUser receiver) {
        mPanelView.sendGift(gift, giftCount, receiver);
    }

    public interface OnGiftListener {
        void onRecharge(GiftListPanel view);

        void onSendGift(GiftListPanel view, Gift gift, int giftCount);
    }
}
