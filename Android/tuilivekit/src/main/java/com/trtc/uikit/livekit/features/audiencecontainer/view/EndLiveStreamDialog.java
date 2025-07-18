package com.trtc.uikit.livekit.features.audiencecontainer.view;

import static com.trtc.uikit.livekit.features.audiencecontainer.state.CoGuestState.CoGuestStatus.NONE;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.Gravity;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.AudienceManager;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

@SuppressLint("ViewConstructor")
public class EndLiveStreamDialog extends PopupDialog {

    protected     Context             mContext;
    private final LiveCoreView    mLiveStream;
    private final AudienceManager mAudienceManager;
    private       LinearLayout    mRootLayout;
    private       OnExitClickListener mOnExitClickListener;

    public EndLiveStreamDialog(@NonNull Context context, LiveCoreView liveStream, AudienceManager liveManager,
                               OnExitClickListener listener) {
        super(context);
        mContext = context;
        mLiveStream = liveStream;
        mAudienceManager = liveManager;
        mOnExitClickListener = listener;
        initView();
    }

    @SuppressLint("InflateParams")
    protected void initView() {
        mRootLayout = new LinearLayout(mContext);
        mRootLayout.setOrientation(LinearLayout.VERTICAL);
        mRootLayout.setBackgroundResource(R.drawable.livekit_popup_dialog_bg);

        initTitleView();
        initExitCoGuestItem();
        initExitRoomItem();
        initCancelItem();
        setView(mRootLayout);
    }

    private void initTitleView() {
        String tips = getContext().getString(R.string.common_audience_end_link_tips);
        TextView titleView = addItemView();
        titleView.setTextSize(12);
        titleView.setText(tips);
        addSplitLine(ScreenUtil.dip2px(1));
    }

    private void initExitCoGuestItem() {
        TextView itemView = addItemView();
        int color = mContext.getResources().getColor(R.color.common_not_standard_red);
        itemView.setTextColor(color);
        itemView.setText(getContext().getString(R.string.common_end_link));
        itemView.setOnClickListener(v -> {
            mLiveStream.terminateIntraRoomConnection();
            mAudienceManager.getCoGuestManager().updateCoGuestStates(NONE);
            dismiss();
        });
        addSplitLine(ScreenUtil.dip2px(1));
    }

    private void initExitRoomItem() {
        TextView itemView = addItemView();
        itemView.setText(getContext().getString(R.string.common_end_live));
        itemView.setOnClickListener(v -> {
            dismiss();
            if (mOnExitClickListener != null) {
                mOnExitClickListener.onClick(v);
            }
        });
        addSplitLine(ScreenUtil.dip2px(7));
    }

    private void initCancelItem() {
        TextView itemView = addItemView();
        itemView.setText(getContext().getString(R.string.common_cancel));
        itemView.setOnClickListener(v -> dismiss());
    }

    private void addSplitLine(int height) {
        View view = new View(mContext);
        int color = mContext.getResources().getColor(R.color.common_design_standard_g8);
        view.setBackgroundColor(color);
        LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT,
                height);
        mRootLayout.addView(view, params);
    }

    private TextView addItemView() {
        TextView textView = new TextView(mContext);
        int color = mContext.getResources().getColor(R.color.common_design_standard_g2);
        textView.setTextColor(color);
        textView.setTextSize(16);
        textView.setGravity(Gravity.CENTER);
        LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(
                LinearLayout.LayoutParams.MATCH_PARENT, ScreenUtil.dip2px(56));
        mRootLayout.addView(textView, params);
        return textView;
    }

    public interface OnExitClickListener {
        void onClick(View view);
    }
}