package com.trtc.uikit.component.gift.view.animation.manager;

import android.os.Handler;
import android.os.Looper;
import android.util.Log;

import com.trtc.uikit.component.gift.store.model.Gift;

import java.util.ArrayList;
import java.util.List;

public class GiftAnimationManager {

    private static final String TAG = "GiftAnimationManager";

    private static final int MAX_CACHE_SIZE               = 3;
    private static final int ANIMATION_PREPARE_TIMEOUT_MS = 2000;

    private final Handler         mMainHandler     = new Handler(Looper.getMainLooper());
    private final List<GiftModel> mGiftPrepareList = new ArrayList<>();

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

    public void add(GiftModel gift) {
        if (gift.isFromSelf) {
            if (mGiftPrepareList.isEmpty()) {
                mGiftPrepareList.add(gift);
            } else {
                // Add after the last gift which is from self.
                int lastIndex = 0;
                for (int i = mGiftPrepareList.size() - 1; i > 0; i--) {
                    GiftModel giftModel = mGiftPrepareList.get(i);
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
                GiftModel giftModel = mGiftPrepareList.get(i);
                if (!giftModel.isFromSelf) {
                    removeIndex = i;
                    break;
                }
            }
            mGiftPrepareList.remove(removeIndex);
        }
        if (mGiftPrepareList.size() == 1 && !mIsPlaying) {
            preparePlay(mGiftPrepareList.remove(0).gift);
        }
    }

    public void startPlay(String animationUrl) {
        Log.i(TAG, "startPlay:" + animationUrl);
        mMainHandler.removeCallbacks(mPrepareTimeoutTask);
        if (mAnimationPlayer != null) {
            mAnimationPlayer.startPlay(animationUrl);
        }
    }

    public void stopPlay() {
        Log.i(TAG, "stopPlay");
        if (mAnimationPlayer != null) {
            mAnimationPlayer.stopPlay();
        }
    }

    private void preparePlay(Gift gift) {
        Log.i(TAG, "preparePlay:" + gift.animationUrl);
        mIsPlaying = true;
        if (mAnimationPlayer != null) {
            mAnimationPlayer.preparePlay(gift);
        }
        mMainHandler.postDelayed(mPrepareTimeoutTask, ANIMATION_PREPARE_TIMEOUT_MS);
    }

    private void onFinished(int error) {
        Log.i(TAG, "onFinished:" + error);
        mIsPlaying = false;
        if (!mGiftPrepareList.isEmpty()) {
            preparePlay(mGiftPrepareList.remove(0).gift);
        }
    }

    public static final class GiftModel {
        public Gift    gift;
        public boolean isFromSelf = false;
    }
}
