package com.trtc.tuikit.common.ui;

import android.app.Dialog;
import android.content.Context;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.constraintlayout.widget.ConstraintLayout;

import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.tuikit.common.R;

public class ConfirmWithCheckboxDialog extends Dialog {
    private Context                                mContext;
    private View                                   mDivideLine;
    private String                                 mTitleText;
    private String                                 mContentText;
    private String                                 mCheckboxText;
    private String                                 mPositiveText;
    private String                                 mNegativeText;
    private CompoundButton.OnCheckedChangeListener mCheckChangedListener;
    private View.OnClickListener                   mPositiveClickListener;
    private View.OnClickListener                   mNegativeClickListener;

    public ConfirmWithCheckboxDialog(@NonNull Context context) {
        super(context, R.style.TUICommonConfirmDialogTheme);
        mContext = context;
    }

    public ConfirmWithCheckboxDialog(@NonNull Context context, int theme) {
        super(context, theme);
        mContext = context;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.common_confirm_dialog);
        setCancelable(false);
        mDivideLine = findViewById(R.id.vertical_divider);
        initText();
        initCheckbox();
        initButtonPositive();
        initButtonNegative();
    }

    private void initText() {
        TextView title = findViewById(R.id.title);
        TextView message = findViewById(R.id.content);

        title.setText(mTitleText);
        message.setText(mContentText);
        if (mTitleText == null) {
            title.setVisibility(View.GONE);
            message.setLayoutParams(setTextLayoutParams());
        } else if (mContentText == null) {
            message.setVisibility(View.GONE);
            title.setLayoutParams(setTextLayoutParams());
        }
    }

    private ConstraintLayout.LayoutParams setTextLayoutParams() {
        ConstraintLayout.LayoutParams layoutParams = new ConstraintLayout.LayoutParams(
                ConstraintLayout.LayoutParams.WRAP_CONTENT,
                ConstraintLayout.LayoutParams.WRAP_CONTENT
        );
        int margin = (int) ScreenUtil.dp2px(37f, mContext.getResources().getDisplayMetrics());
        layoutParams.topToTop = ConstraintLayout.LayoutParams.PARENT_ID;
        layoutParams.bottomToTop = mCheckChangedListener == null
                ? R.id.horizontal_divider : R.id.check;
        layoutParams.setMargins(0, margin, 0, margin);
        return layoutParams;
    }

    private void initCheckbox() {
        CheckBox checkBox = findViewById(R.id.check);

        if (mCheckChangedListener == null) {
            checkBox.setVisibility(View.GONE);
        }
        if (!TextUtils.isEmpty(mCheckboxText)) {
            checkBox.setText(mCheckboxText);
        }
        checkBox.setOnCheckedChangeListener(mCheckChangedListener);
    }

    private void initButtonPositive() {
        Button buttonPositive = findViewById(R.id.btn_positive);

        if (mPositiveClickListener == null) {
            buttonPositive.setVisibility(View.GONE);
            mDivideLine.setVisibility(View.GONE);
            return;
        }
        if (!TextUtils.isEmpty(mPositiveText)) {
            buttonPositive.setText(mPositiveText);
        }
        buttonPositive.setOnClickListener(mPositiveClickListener);
    }

    private void initButtonNegative() {
        Button buttonNegative = findViewById(R.id.btn_negative);

        if (mNegativeClickListener == null) {
            buttonNegative.setVisibility(View.GONE);
            mDivideLine.setVisibility(View.GONE);
            return;
        }
        if (!TextUtils.isEmpty(mNegativeText)) {
            buttonNegative.setText(mNegativeText);
        }
        buttonNegative.setOnClickListener(mNegativeClickListener);
    }

    public void setTitle(String title) {
        mTitleText = title;
    }

    public void setContent(String content) {
        mContentText = content;
    }

    public void setCheckboxText(String text, CompoundButton.OnCheckedChangeListener listener) {
        mCheckboxText = text;
        mCheckChangedListener = listener;
    }

    public void setPositiveText(String text, View.OnClickListener listener) {
        mPositiveText = text;
        mPositiveClickListener = listener;
    }

    public void setNegativeText(String text, View.OnClickListener listener) {
        mNegativeText = text;
        mNegativeClickListener = listener;
    }
}