package com.trtc.uikit.livekit.component.gift.view.animation.manager;

import android.os.Handler;
import android.os.Looper;
import android.util.Log;

import java.util.ArrayList;
import java.util.List;

public class GiftAnimationManager {

    private static final String TAG = "GiftAnimationManager";

    private static final int MAX_CACHE_SIZE               = 3;
    private static final int ANIMATION_PREPARE_TIMEOUT_MS = 2000;

    private final Handler                  mMainHandler     = new Handler(Looper.getMainLooper());
    private final List<GiftAnimationModel> mGiftPrepareList = new ArrayList<>();

    private AnimationPlayer mAnimationPlayer;
    private boolean         mIsPlaying;

    private final Runnable mPrepareTimeoutTask = () -> {
        Log.w(TAG, "Prepare timeout");
        onFinished(-1);
    };

    public GiftAnimationManager() {
    }

    public void setPlayer(AnimationPlayer player) {
        mAnimationPlayer = player;
        mAnimationPlayer.setCallback(this::onFinished);
    }

    public void add(GiftAnimationModel gift) {
        if (gift.isFromSelf) {
            if (mGiftPrepareList.isEmpty()) {
                mGiftPrepareList.add(gift);
            } else {
                // Add after the last gift which is from self.
                int lastIndex = 0;
                for (int i = mGiftPrepareList.size() - 1; i > 0; i--) {
                    GiftAnimationModel giftModel = mGiftPrepareList.get(i);
                    if (giftModel.isFromSelf) {
                        lastIndex = i;
                        break;
                    }
                }
                mGiftPrepareList.add(lastIndex + 1, gift);
            }
        } else {
            mGiftPrepareList.add(gift);
        }
        // remove redundant gift
        if (mGiftPrepareList.size() == MAX_CACHE_SIZE + 1) {
            int removeIndex = 1;
            for (int i = 1; i < mGiftPrepareList.size(); i++) {
                GiftAnimationModel giftModel = mGiftPrepareList.get(i);
                if (!giftModel.isFromSelf) {
                    removeIndex = i;
                    break;
                }
            }
            mGiftPrepareList.remove(removeIndex);
        }
        if (mGiftPrepareList.size() == 1 && !mIsPlaying) {
            preparePlay(mGiftPrepareList.remove(0));
        }
    }

    public void startPlay(GiftAnimationModel model) {
        Log.i(TAG, "startPlay:" + model.gift.resourceUrl);
        mMainHandler.removeCallbacks(mPrepareTimeoutTask);
        if (mAnimationPlayer != null) {
            mAnimationPlayer.startPlay(model);
        }
    }

    public void stopPlay() {
        Log.i(TAG, "stopPlay");
        if (mAnimationPlayer != null) {
            mAnimationPlayer.stopPlay();
        }
    }

    private void preparePlay(GiftAnimationModel giftModel) {
        Log.i(TAG, "preparePlay:" + giftModel.gift.resourceUrl);
        mIsPlaying = true;
        if (mAnimationPlayer != null) {
            mAnimationPlayer.preparePlay(giftModel);
        }
        mMainHandler.postDelayed(mPrepareTimeoutTask, ANIMATION_PREPARE_TIMEOUT_MS);
    }

    private void onFinished(int error) {
        Log.i(TAG, "onFinished:" + error);
        mIsPlaying = false;
        if (!mGiftPrepareList.isEmpty()) {
            preparePlay(mGiftPrepareList.remove(0));
        }
    }
}
