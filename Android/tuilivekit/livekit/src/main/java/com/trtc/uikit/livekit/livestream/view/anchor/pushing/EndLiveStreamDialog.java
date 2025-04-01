package com.trtc.uikit.livekit.livestream.view.anchor.pushing;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.text.TextUtils;
import android.view.Gravity;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.manager.api.LiveStreamLog;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

@SuppressLint("ViewConstructor")
public class EndLiveStreamDialog extends PopupDialog {

    protected     Context           mContext;
    private final LiveCoreView      mLiveStream;
    private final LiveStreamManager mLiveStreamManager;
    private       LinearLayout      mRootLayout;

    public EndLiveStreamDialog(@NonNull Context context, LiveCoreView liveStream, LiveStreamManager liveManager) {
        super(context);
        mContext = context;
        mLiveStream = liveStream;
        mLiveStreamManager = liveManager;
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
        return mLiveStreamManager.getCoGuestState().connectedUserList.getValue().size() > 1;
    }

    private boolean isInCoHost() {
        return !mLiveStreamManager.getCoHostState().connectedUsers.getValue().isEmpty();
    }

    private boolean isInBattle() {
        return Boolean.TRUE.equals(mLiveStreamManager.getBattleState().mIsBattleRunning.getValue());
    }

    private void initTitleView() {
        String tips = "";
        if (isInCoGuest()) {
            tips = getContext().getString(R.string.live_anchor_end_link_tips);
        } else if (isInCoHost()) {
            tips = getContext().getString(R.string.live_end_connection_tips);
            if (isInBattle()) {
                tips = getContext().getString(R.string.live_end_pk_tips);
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
        int color = mContext.getResources().getColor(R.color.livekit_not_standard_red);
        itemView.setTextColor(color);
        itemView.setText(getContext().getString(R.string.live_end_pk));
        itemView.setOnClickListener(v -> {
            String battleId = mLiveStreamManager.getBattleState().mBattleId;
            mLiveStream.terminateBattle(battleId, new TUIRoomDefine.ActionCallback() {
                @Override
                public void onSuccess() {
                    mLiveStreamManager.getBattleManager().onExitBattle();
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    LiveStreamLog.error("EndLiveStreamDialog" + " terminateBattle " +
                            "failed:error:" + error + "," + "errorCode:" + error.getValue() + "message:" + message);
                }
            });
            dismiss();
        });
        addSplitLine(ScreenUtil.dip2px(1));
    }

    private void initExitCoHostItem() {
        TextView itemView = addItemView();
        int color = mContext.getResources().getColor(R.color.livekit_not_standard_red);
        itemView.setTextColor(color);
        itemView.setText(getContext().getString(R.string.live_end_connection));
        itemView.setOnClickListener(v -> {
            mLiveStream.terminateCrossRoomConnection();
            mLiveStreamManager.getCoHostManager().cleanConnectedUsers();
            dismiss();
        });
        addSplitLine(ScreenUtil.dip2px(1));
    }

    private void initExitRoomItem() {
        TextView itemView = addItemView();
        itemView.setText(getContext().getString(R.string.live_end_live));
        itemView.setOnClickListener(v -> {
            if (!v.isEnabled()) {
                return;
            }
            v.setEnabled(false);
            int maxAudienceCount = mLiveStreamManager.getRoomState().maxAudienceCount;
            mLiveStreamManager.getDashboardManager().updateMaxViewersCount(maxAudienceCount);
            mLiveStreamManager.getRoomManager().updateLiveStatus(RoomState.LiveStatus.DASHBOARD);
            mLiveStreamManager.getDashboardManager().getLiveInfo(new TUILiveListManager.LiveInfoCallback() {
                @Override
                public void onSuccess(TUILiveListManager.LiveInfo liveInfo) {
                    if (checkActivityStatus()) {
                        mLiveStreamManager.getDashboardManager().updateMaxViewersCount(liveInfo.viewCount);
                        stopLiveStream();
                    }
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    LiveStreamLog.error("EndLiveStreamDialog" + " getLiveInfo " +
                            "failed:error:" + error + "," + "errorCode:" + error.getValue() + "message:" + message);
                    if (checkActivityStatus()) {
                        stopLiveStream();
                        ErrorLocalized.onError(error);
                    }
                }
            });
            dismiss();
        });
        addSplitLine(ScreenUtil.dip2px(7));
    }

    private boolean checkActivityStatus() {
        Activity activity = (Activity) mContext;
        return !activity.isFinishing() && !activity.isDestroyed();
    }

    private void stopLiveStream() {
        mLiveStream.stopLiveStream(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {

            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error("EndLiveStreamDialog" + " stopLiveStream " +
                        "failed:error:" + error + "," + "errorCode:" + error.getValue() + "message:" + message);
                if (checkActivityStatus()) {
                    ErrorLocalized.onError(error);
                }
            }
        });
    }

    private void initCancelItem() {
        TextView itemView = addItemView();
        itemView.setText(getContext().getString(R.string.live_cancel));
        itemView.setOnClickListener(v -> dismiss());
    }

    private void addSplitLine(int height) {
        View view = new View(mContext);
        int color = mContext.getResources().getColor(R.color.livekit_design_standard_g8);
        view.setBackgroundColor(color);
        LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT,
                height);
        mRootLayout.addView(view, params);
    }

    private TextView addItemView() {
        TextView textView = new TextView(mContext);
        int color = mContext.getResources().getColor(R.color.livekit_design_standard_g2);
        textView.setTextColor(color);
        textView.setTextSize(16);
        textView.setGravity(Gravity.CENTER);
        LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(
                LinearLayout.LayoutParams.MATCH_PARENT, ScreenUtil.dip2px(56));
        mRootLayout.addView(textView, params);
        return textView;
    }
}
