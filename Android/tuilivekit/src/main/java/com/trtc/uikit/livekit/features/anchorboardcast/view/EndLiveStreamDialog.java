package com.trtc.uikit.livekit.features.anchorboardcast.view;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.Gravity;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

@SuppressLint("ViewConstructor")
public class EndLiveStreamDialog extends PopupDialog {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("EndLiveStreamDialog");

    protected     Context                     mContext;
    private final LiveCoreView                mCoreView;
    private final AnchorManager               mAnchorManager;
    private       LinearLayout                mRootLayout;
    private final EndLiveStreamDialogListener mListener;

    public EndLiveStreamDialog(@NonNull Context context, LiveCoreView liveStream, AnchorManager liveManager,
                               EndLiveStreamDialogListener listener) {
        super(context);
        mContext = context;
        mCoreView = liveStream;
        mAnchorManager = liveManager;
        mListener = listener;
        initView();
    }

    @SuppressLint("InflateParams")
    protected void initView() {
        mRootLayout = new LinearLayout(mContext);
        mRootLayout.setOrientation(LinearLayout.VERTICAL);
        mRootLayout.setBackgroundResource(R.drawable.livekit_popup_dialog_bg);

        initTitleView();
        if (isInCoHost()) {
            if (isInBattle()) {
                initExitBattleItem();
            } else {
                initExitCoHostItem();
            }
        }
        initExitRoomItem();
        initCancelItem();
        setView(mRootLayout);
    }

    private boolean isInCoGuest() {
        return mAnchorManager.getCoreState().coGuestState.connectedUserList.getValue().size() > 1;
    }

    private boolean isInCoHost() {
        return !mAnchorManager.getCoreState().coHostState.connectedUserList.getValue().isEmpty();
    }

    private boolean isInBattle() {
        return Boolean.TRUE.equals(mAnchorManager.getBattleState().mIsBattleRunning.getValue());
    }

    private void initTitleView() {
        String tips = "";
        if (isInCoGuest()) {
            tips = getContext().getString(R.string.common_anchor_end_link_tips);
        } else if (isInCoHost()) {
            tips = getContext().getString(R.string.common_end_connection_tips);
            if (isInBattle()) {
                tips = getContext().getString(R.string.common_end_pk_tips);
            }
        }
        if (TextUtils.isEmpty(tips)) {
            return;
        }
        TextView titleView = addItemView();
        titleView.setTextSize(12);
        titleView.setText(tips);
        addSplitLine(ScreenUtil.dip2px(1));
    }

    private void initExitBattleItem() {
        TextView itemView = addItemView();
        int color = mContext.getResources().getColor(R.color.common_not_standard_red);
        itemView.setTextColor(color);
        itemView.setText(getContext().getString(R.string.common_end_pk));
        itemView.setOnClickListener(v -> {
            String battleId = mAnchorManager.getBattleState().mBattleId;
            mCoreView.terminateBattle(battleId, new TUIRoomDefine.ActionCallback() {
                @Override
                public void onSuccess() {
                    mAnchorManager.getBattleManager().onExitBattle();
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    LOGGER.error("terminateBattle failed:error:" + error + "," + "errorCode:" + error.getValue() +
                            "message:" + message);
                }
            });
            dismiss();
        });
        addSplitLine(ScreenUtil.dip2px(1));
    }

    private void initExitCoHostItem() {
        TextView itemView = addItemView();
        int color = mContext.getResources().getColor(R.color.common_not_standard_red);
        itemView.setTextColor(color);
        itemView.setText(getContext().getString(R.string.common_end_connection));
        itemView.setOnClickListener(v -> {
            mCoreView.terminateCrossRoomConnection();
            dismiss();
        });
        addSplitLine(ScreenUtil.dip2px(1));
    }

    private void initExitRoomItem() {
        TextView itemView = addItemView();
        itemView.setText(getContext().getString(R.string.common_end_live));
        itemView.setOnClickListener(this::onExitLiveClick);
        addSplitLine(ScreenUtil.dip2px(7));
    }

    private void onExitLiveClick(View view) {
        if (!view.isEnabled()) {
            return;
        }
        view.setEnabled(false);
        mAnchorManager.loadLiveEndInfo(new Runnable() {
            @Override
            public void run() {
                mRootLayout.post(this::onLoadEnd);
            }

            private void onLoadEnd() {
                mCoreView.stopLiveStream(null);
                if (mListener != null) {
                    mListener.onRoomExitEndStatistics();
                }
                dismiss();
                mAnchorManager.notifyRoomExit();
                mCoreView.setLocalVideoMuteImage(null, null);
            }
        });
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

    public interface EndLiveStreamDialogListener {
        void onRoomExitEndStatistics();
    }
}
