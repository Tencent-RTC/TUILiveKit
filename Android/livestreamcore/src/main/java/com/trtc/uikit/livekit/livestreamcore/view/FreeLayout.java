package com.trtc.uikit.livekit.livestreamcore.view;

import static android.view.View.MeasureSpec.EXACTLY;

import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.util.Log;
import android.view.View;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.uikit.livekit.livestreamcore.common.utils.ColorUtil;
import com.trtc.uikit.livekit.livestreamcore.view.viewmodel.LayoutConfig;
import com.trtc.uikit.livekit.livestreamcore.view.viewmodel.LayoutInfo;
import com.trtc.uikit.livekit.livestreamcore.view.viewmodel.ViewInfo;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

public class FreeLayout extends FrameLayout {
    private static final String TAG             = "FreeLayout";
    private static final double FULLSCREEN_MODE = -1;

    private String       mLayoutJson   = "";
    private LayoutConfig mLayoutConfig = null;
    private int          mScreenWidth  = 1080;
    private int          mScreenHeight = 2400;

    public FreeLayout(@NonNull Context context) {
        this(context, null);
    }

    public FreeLayout(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public FreeLayout(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);

        mScreenWidth = ScreenUtil.getRealScreenWidth(context);
        mScreenHeight = ScreenUtil.getRealScreenHeight(context);
    }

    public void setLayout(String json) {
        mLayoutJson = json;
        mLayoutConfig = LayoutConfig.parseJson(json);
        requestLayout();
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

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);
        mScreenWidth = getMeasuredWidth();
        mScreenHeight = getMeasuredHeight();
        if (TextUtils.isEmpty(mLayoutJson)) {
            return;
        }
        int childViewCount = getChildCount();
        LayoutInfo layoutInfo = mLayoutConfig.getLayoutInfo(childViewCount);

        if (layoutInfo == null || layoutInfo.viewInfoList.isEmpty()) {
            return;
        }
        setBackgroundColor(ColorUtil.parseHex(layoutInfo.backgroundColor));
        for (int i = 0; i < childViewCount && i < layoutInfo.viewInfoList.size(); i++) {
            final View child = getChildAt(i);
            final ViewInfo viewInfo = layoutInfo.viewInfoList.get(i);
            child.setBackgroundColor(ColorUtil.parseHex(viewInfo.backgroundColor));
            if (child.getVisibility() != GONE) {
                int viewWidth = (int) (viewInfo.width * mScreenWidth);
                int viewHeight = viewInfo.height == FULLSCREEN_MODE ? mScreenHeight :
                        (int) (viewInfo.height * mScreenWidth);
                int measureWidth = MeasureSpec.makeMeasureSpec(viewWidth, EXACTLY);
                int measureHeight = MeasureSpec.makeMeasureSpec(viewHeight, EXACTLY);
                child.measure(measureWidth, measureHeight);
            }
        }
    }

    @Override
    protected void onLayout(boolean changed, int l, int t, int r, int b) {
        if (TextUtils.isEmpty(mLayoutJson)) {
            super.onLayout(changed, l, t, r, b);
            return;
        }
        int childViewCount = getChildCount();
        LayoutInfo layoutInfo = mLayoutConfig.getLayoutInfo(childViewCount);
        if (layoutInfo == null || layoutInfo.viewInfoList.isEmpty()) {
            return;
        }
        int count = getChildCount();
        View child;
        for (int i = 0; i < count && i < layoutInfo.viewInfoList.size(); i++) {
            child = getChildAt(i);
            ViewInfo viewInfo = layoutInfo.viewInfoList.get(i);
            int left = (int) (viewInfo.x * mScreenWidth);
            int top = (int) (viewInfo.y * mScreenWidth);

            int right = left + child.getMeasuredWidth();
            int bottom = top + child.getMeasuredHeight();
            child.setZ(viewInfo.zOrder * 1.0f);
            child.layout(left, top, right, bottom);
        }
    }
}
