package com.trtc.uikit.livekit.livestreamcore.view;

import android.content.Context;
import android.graphics.Rect;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.util.Log;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.uikit.livekit.livestreamcore.view.viewmodel.LayoutConfig;
import com.trtc.uikit.livekit.livestreamcore.view.viewmodel.LayoutInfo;
import com.trtc.uikit.livekit.livestreamcore.view.viewmodel.ViewInfo;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

public class BattleContainLayout extends FrameLayout {
    private static final String TAG             = "BattleContainLayout";
    private static final double FULLSCREEN_MODE = -1;

    private String       mLayoutJson   = "";
    private LayoutConfig mLayoutConfig = null;

    private final Rect mRect = new Rect();

    private static final int FULL_SCREEN_LAYOUT_TOP_EXTRA = 18;

    private int mViewCount = 1;

    public BattleContainLayout(@NonNull Context context) {
        this(context, null);
    }

    public BattleContainLayout(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public BattleContainLayout(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public void setLayout(String json) {
        mLayoutJson = json;
        mLayoutConfig = LayoutConfig.parseJson(json);
    }

    public void setViewCount(int viewCount) {
        mViewCount = viewCount;
        Log.i(TAG, "setViewCount:" + viewCount);
    }

    public void setLayoutResource(int resourceId) {
        try (InputStream inputStream = getResources().openRawResource(resourceId);
             BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8))) {

            StringBuilder jsonBuilder = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                jsonBuilder.append(line);
            }
            setLayout(jsonBuilder.toString());
        } catch (Exception e) {
            Log.e(TAG, "setLayoutResource Exception:" + e);
        }
    }

    public void resize(int baseWidth, int baseHeight) {
        if (TextUtils.isEmpty(mLayoutJson)) {
            return;
        }
        LayoutInfo layoutInfo = mLayoutConfig.getLayoutInfo(mViewCount);
        if (layoutInfo == null || layoutInfo.viewInfoList.isEmpty()) {
            return;
        }
        mRect.top = baseHeight;
        mRect.bottom = 0;
        mRect.left = baseWidth;
        mRect.right = 0;
        for (int i = 0; i < layoutInfo.viewInfoList.size(); i++) {
            final ViewInfo viewInfo = layoutInfo.viewInfoList.get(i);
            int viewWidth = (int) (viewInfo.width * baseWidth);
            int viewHeight = viewInfo.height == FULLSCREEN_MODE ? baseHeight :
                    (int) (viewInfo.height * baseWidth);
            mRect.top = (int) Math.min(mRect.top, viewInfo.y * baseWidth);
            mRect.bottom = (int) Math.max(mRect.bottom, viewInfo.y * baseWidth + viewHeight);
            mRect.left = (int) Math.min(mRect.left, viewInfo.x * baseWidth);
            mRect.right = (int) Math.max(mRect.right, viewInfo.x * baseWidth + viewWidth);
        }

        mRect.top -= ScreenUtil.dip2px(FULL_SCREEN_LAYOUT_TOP_EXTRA);

        LayoutParams params = (LayoutParams) getLayoutParams();
        params.width = mRect.right - mRect.left;
        params.height = mRect.bottom - mRect.top;
        params.topMargin = mRect.top;
        params.leftMargin = mRect.left;
        setLayoutParams(params);
    }
}
