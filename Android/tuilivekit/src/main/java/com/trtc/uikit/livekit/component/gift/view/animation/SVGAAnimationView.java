package com.trtc.uikit.livekit.component.gift.view.animation;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;

import android.content.Context;
import android.util.AttributeSet;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.opensource.svgaplayer.SVGACallback;
import com.opensource.svgaplayer.SVGAImageView;
import com.opensource.svgaplayer.SVGAParser;
import com.opensource.svgaplayer.SVGAVideoEntity;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;

public final class SVGAAnimationView extends AnimationView implements SVGACallback {

    private static final String TAG = "SVGAAnimationView";

    private final SVGAParser    mSVGAParser;
    private final SVGAImageView mImageView;

    public SVGAAnimationView(@NonNull Context context) {
        this(context, null);
    }

    public SVGAAnimationView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        mImageView = new SVGAImageView(context);
        LayoutParams params = new LayoutParams(MATCH_PARENT, MATCH_PARENT);
        addView(mImageView, params);
        mImageView.setLoops(1);
        mSVGAParser = SVGAParser.Companion.shareParser();
        mSVGAParser.init(context);
    }

    @Override
    public void playAnimation(String playUrl) {
        InputStream stream = openInputStream(playUrl);
        if (stream == null) {
            Log.e(TAG, "InputStream is null");
            if (mCallback != null) {
                mCallback.onFinished(-1);
            }
            return;
        }
        mSVGAParser.decodeFromInputStream(stream, "", new SVGAParser.ParseCompletion() {
            @Override
            public void onComplete(@NonNull SVGAVideoEntity svgaVideoEntity) {
                mImageView.setVisibility(VISIBLE);
                mImageView.setVideoItem(svgaVideoEntity);
                mImageView.startAnimation();
            }

            @Override
            public void onError() {
                Log.e(TAG, "decodeFromURL onError");
                if (mCallback != null) {
                    mCallback.onFinished(-1);
                }
            }
        }, true, null, "");
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        mImageView.setCallback(this);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        mImageView.setCallback(null);
        mImageView.stopAnimation(true);
    }

    @Override
    public void onFinished() {
        mImageView.setVisibility(GONE);
        if (mCallback != null) {
            mCallback.onFinished(0);
        }
    }

    @Override
    public void onPause() {

    }

    @Override
    public void onRepeat() {

    }

    @Override
    public void onStep(int i, double v) {

    }

    private InputStream openInputStream(String path) {
        try {
            File file = new File(path);
            if (file.exists()) {
                return new FileInputStream(file);
            }
        } catch (FileNotFoundException e) {
            Log.i(TAG, " " + e.getLocalizedMessage());
        }
        return null;
    }
}
