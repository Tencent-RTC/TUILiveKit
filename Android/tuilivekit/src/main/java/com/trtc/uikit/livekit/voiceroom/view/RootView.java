package com.trtc.uikit.livekit.voiceroom.view;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;
import static android.view.ViewGroup.LayoutParams.WRAP_CONTENT;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_COUNT;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_ICON_URL;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_NAME;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_RECEIVER_USERNAME;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_VIEW_TYPE_1;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveController;
import com.trtc.uikit.livekit.common.core.controller.RoomController;
import com.trtc.uikit.livekit.common.core.store.state.operation.RoomState;
import com.trtc.uikit.livekit.common.uicomponent.barrage.TUIBarrageDisplayView;
import com.trtc.uikit.livekit.common.uicomponent.barrage.model.TUIBarrage;
import com.trtc.uikit.livekit.common.uicomponent.gift.TUIGiftPlayView;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGift;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGiftUser;
import com.trtc.uikit.livekit.common.uicomponent.gift.service.GiftCacheService;
import com.trtc.uikit.livekit.common.uicomponent.gift.view.GiftBarrageAdapter;
import com.trtc.uikit.livekit.common.utils.Constants;
import com.trtc.uikit.livekit.common.view.AnchorEndView;
import com.trtc.uikit.livekit.common.view.AudienceEndView;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.voiceroom.model.MenuDataGenerate;
import com.trtc.uikit.livekit.voiceroom.view.bottommenu.BottomMenuView;
import com.trtc.uikit.livekit.voiceroom.view.preview.AnchorPreviewView;
import com.trtc.uikit.livekit.voiceroom.view.seatview.SeatListView;
import com.trtc.uikit.livekit.voiceroom.view.topview.TopView;

@SuppressLint("ViewConstructor")
public class RootView extends BasicView {
    private RelativeLayout mLayoutTopViewContainer;
    private RelativeLayout mLayoutSeatListContainer;
    private RelativeLayout mLayoutBottomMenuContainer;
    private RelativeLayout mLayoutBarrageContainer;
    private RelativeLayout mLayoutAnchorPreviewViewContainer;
    private RelativeLayout mLayoutGiftContainer;
    private RelativeLayout mLayoutEndViewContainer;

    private ImageView             mRootBg;
    private TUIBarrageDisplayView mBarrageDisplayView;
    private TUIGiftPlayView       mGiftPlayView;

    private final Observer<String> mCoverURLObserver = this::updateRoomCover;

    private final Observer<Boolean> mShowAnchorPreviewObserver = this::updateAnchorPreviewView;

    private final GiftCacheService mGiftCacheService;

    private final Observer<Boolean> mShowEndViewObserver = this::updateEndView;

    private final Observer<RoomState.EnterRoomSate> mEnterRoomSateObserver = state -> {
        if (state == RoomState.EnterRoomSate.IN_ROOM) {
            initBottomMenuView();
            initTopView();
            initBarrageView();
            initGiftView();
        }
    };

    public RootView(@NonNull Context context, LiveController liveController) {
        super(context, liveController);
        mGiftCacheService = new GiftCacheService(context);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_root_view, this, true);
        mRootBg = findViewById(R.id.root_bg);
        mLayoutAnchorPreviewViewContainer = findViewById(R.id.rl_anchor_preview_view);
        mLayoutTopViewContainer = findViewById(R.id.rl_top_view);
        mLayoutSeatListContainer = findViewById(R.id.rl_seat_list);
        mLayoutBottomMenuContainer = findViewById(R.id.rl_bottom_menu);
        mLayoutBarrageContainer = findViewById(R.id.rl_barrage);
        mLayoutGiftContainer = findViewById(R.id.rl_gift);
        mLayoutEndViewContainer = findViewById(R.id.rl_end_view);
        initSeatListView();
        updateAnchorPreviewView(mViewState.showAnchorPreview.get());
        updateRoomCover(mRoomState.coverURL.get());
    }

    @Override
    protected void addObserver() {
        mRoomState.coverURL.observe(mCoverURLObserver);
        mRoomState.enterRoomState.observe(mEnterRoomSateObserver);
        mViewState.showAnchorPreview.observe(mShowAnchorPreviewObserver);
        mViewState.showEndView.observe(mShowEndViewObserver);
    }

    @Override
    protected void removeObserver() {
        mRoomState.coverURL.remove(mCoverURLObserver);
        mRoomState.enterRoomState.remove(mEnterRoomSateObserver);
        mViewState.showAnchorPreview.remove(mShowAnchorPreviewObserver);
        mViewState.showEndView.removeObserver(mShowEndViewObserver);
    }

    private void initTopView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutTopViewContainer.removeAllViews();
        TopView topView = new TopView(mContext, mLiveController);
        mLayoutTopViewContainer.addView(topView, layoutParams);
    }

    private void initSeatListView() {
        ViewGroup.LayoutParams layoutParams = new ViewGroup.LayoutParams(MATCH_PARENT, WRAP_CONTENT);
        mLayoutSeatListContainer.removeAllViews();
        SeatListView seatListView = new SeatListView(mContext, mLiveController);
        mLayoutSeatListContainer.addView(seatListView, layoutParams);
    }

    private void initBottomMenuView() {
        mLayoutBottomMenuContainer.removeAllViews();
        MenuDataGenerate generate = new MenuDataGenerate(mContext, mLiveController);
        ViewGroup.LayoutParams layoutParams = new ViewGroup.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        BottomMenuView bottomMenuView = new BottomMenuView(mContext, mLiveController,
                generate.generateBottomMenuData());
        mLayoutBottomMenuContainer.addView(bottomMenuView, layoutParams);
    }

    private void updateRoomCover(String url) {
        ImageLoader.load(mContext, mRootBg, url, R.drawable.livekit_voiceroom_cover);
    }

    private void updateAnchorPreviewView(boolean isShow) {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        AnchorPreviewView anchorPreviewView;
        if (isShow) {
            mLayoutAnchorPreviewViewContainer.removeAllViews();
            anchorPreviewView = new AnchorPreviewView(mContext, mLiveController);
            mLayoutAnchorPreviewViewContainer.addView(anchorPreviewView, layoutParams);
        } else {
            mLayoutAnchorPreviewViewContainer.removeAllViews();
        }
    }

    private void initGiftView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutGiftContainer.removeAllViews();
        mGiftPlayView = new TUIGiftPlayView(mContext, mLiveController.getRoomSate().roomId);
        mLayoutGiftContainer.addView(mGiftPlayView, layoutParams);
        mGiftPlayView.setListener(new TUIGiftPlayView.TUIGiftPlayViewListener() {
            @Override
            public void onReceiveGift(TUIGift gift, int giftCount, TUIGiftUser sender, TUIGiftUser receiver) {
                RoomController roomController = mLiveController.getRoomController();
                roomController.updateGiftIncome(gift.price * giftCount + mRoomState.giftIncome);
                roomController.insertGiftPeople(sender.userId);
                TUIBarrage barrage = new TUIBarrage();
                barrage.content = "gift";
                barrage.user.userId = sender.userId;
                barrage.user.userName = sender.userName;
                barrage.user.avatarUrl = sender.avatarUrl;
                barrage.user.level = sender.level;
                barrage.extInfo.put(Constants.GIFT_VIEW_TYPE, GIFT_VIEW_TYPE_1);
                barrage.extInfo.put(GIFT_NAME, gift.giftName);
                barrage.extInfo.put(GIFT_COUNT, giftCount);
                barrage.extInfo.put(GIFT_ICON_URL, gift.imageUrl);
                barrage.extInfo.put(GIFT_RECEIVER_USERNAME, receiver.userName);
                mBarrageDisplayView.insertBarrages(barrage);
            }

            @Override
            public void onPlayGiftAnimation(TUIGiftPlayView view, TUIGift gift) {
                mGiftCacheService.request(gift.animationUrl, (error, result) -> {
                    if (error == 0) {
                        view.playGiftAnimation(result);
                    }
                });
            }
        });
    }

    private void initBarrageView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutBarrageContainer.removeAllViews();
        mBarrageDisplayView = new TUIBarrageDisplayView(mContext, mLiveController.getRoomSate().roomId);
        mLayoutBarrageContainer.addView(mBarrageDisplayView, layoutParams);
        mBarrageDisplayView.setAdapter(new GiftBarrageAdapter(mContext));
    }

    private void initAnchorEndView() {
        mLayoutEndViewContainer.removeAllViews();
        AnchorEndView anchorEndView = new AnchorEndView(mContext, mLiveController);
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutEndViewContainer.addView(anchorEndView, layoutParams);
    }

    private void initAudienceEndView() {
        mLayoutEndViewContainer.removeAllViews();
        AudienceEndView audienceEndView = new AudienceEndView(mContext, mLiveController);
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutEndViewContainer.addView(audienceEndView, layoutParams);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        mGiftCacheService.release();
    }

    private void updateEndView(boolean isShow) {
        if (isShow) {
            showEndView();
        } else {
            mLayoutEndViewContainer.removeAllViews();
        }
    }

    private void showEndView() {
        if (mRoomState.enterRoomState.get() != RoomState.EnterRoomSate.IN_ROOM) {
            return;
        }
        RoomController roomController = mLiveController.getRoomController();
        roomController.updateLikeNumber(mGiftPlayView.getLikeCount());
        roomController.updateMessageCount(mBarrageDisplayView.getBarrageCount());
        if (mUserState.selfInfo.role.get() == TUIRoomDefine.Role.ROOM_OWNER) {
            initAnchorEndView();
        } else {
            initAudienceEndView();
        }
    }
}
