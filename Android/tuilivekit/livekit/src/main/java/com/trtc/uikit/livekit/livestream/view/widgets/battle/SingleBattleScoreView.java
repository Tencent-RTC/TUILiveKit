package com.trtc.uikit.livekit.livestream.view.widgets.battle;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.uikit.livekit.R;

/**
 * mode: 1v1
 * Battle score view
 */
@SuppressLint("ViewConstructor")
public class SingleBattleScoreView extends FrameLayout {

    private final View     mLineDivider;
    private final View     mImageDivider;
    private final TextView mTextScoreLeft;
    private final TextView mTextScoreRight;

    public SingleBattleScoreView(@NonNull Context context) {
        this(context, null);
    }

    public SingleBattleScoreView(@NonNull Context context, AttributeSet attrs) {
        super(context, attrs);
        inflate(getContext(), R.layout.livekit_battle_single_battle_score_view, this);
        mLineDivider = findViewById(R.id.v_divider);
        mImageDivider = findViewById(R.id.iv_divider);
        mTextScoreLeft = findViewById(R.id.tv_score_left);
        mTextScoreRight = findViewById(R.id.tv_score_right);
        mTextScoreLeft.setText("0");
        mTextScoreRight.setText("0");
    }

    public void updateScores(int scoreLeft, int scoreRight) {
        if (scoreLeft + scoreRight < 0 || getWidth() == 0) {
            return;
        }
        mTextScoreLeft.setText(String.valueOf(scoreLeft));
        mTextScoreRight.setText(String.valueOf(scoreRight));
        float textWidthLeft = mTextScoreLeft.getPaint().measureText(mTextScoreLeft.getText().toString())
                + 2 * mTextScoreLeft.getPaddingLeft();
        float textWidthRight = mTextScoreRight.getPaint().measureText(mTextScoreRight.getText().toString())
                + 2 * mTextScoreRight.getPaddingLeft();
        int width = getWidth();
        float ratio = scoreLeft + scoreRight == 0 ? 0.5F : 1.0F * scoreLeft / (scoreLeft + scoreRight);
        int dividerX = (int) (width * ratio);
        dividerX = (int) Math.max(textWidthLeft, dividerX);
        dividerX = (int) Math.min(width - textWidthRight, dividerX);
        updateDivider(dividerX);
    }

    private void updateDivider(int dividerX) {
        RelativeLayout.LayoutParams vDividerParams = (RelativeLayout.LayoutParams) mLineDivider.getLayoutParams();
        vDividerParams.removeRule(RelativeLayout.CENTER_HORIZONTAL);
        vDividerParams.leftMargin = dividerX;
        mLineDivider.setLayoutParams(vDividerParams);

        RelativeLayout.LayoutParams imageDividerParams = (RelativeLayout.LayoutParams) mImageDivider.getLayoutParams();
        imageDividerParams.removeRule(RelativeLayout.CENTER_HORIZONTAL);
        imageDividerParams.leftMargin = dividerX - imageDividerParams.width / 2 - ScreenUtil.dip2px(1F);
        mImageDivider.setLayoutParams(imageDividerParams);

        invalidate();
    }
}
