package com.tencent.liteav.liveroom.ui.widget;

import android.content.Context;
import android.content.res.TypedArray;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.tencent.liteav.liveroom.R;


public class CustomTitle extends RelativeLayout {

    private String  mTitleText;
    private String  mBackText;
    private String  mMoreText;
    private boolean mCanBack;

    private TextView mTextBack;
    private TextView mTextTitle;
    private TextView mTextMore;

    public CustomTitle(Context context, AttributeSet attrs) {
        super(context, attrs);
        LayoutInflater.from(context).inflate(R.layout.trtcliveroom_view_title, this);
        TypedArray ta = context.obtainStyledAttributes(attrs, R.styleable.TRTCLiveRoomCustomTitle, 0, 0);
        try {
            mTitleText = ta.getString(R.styleable.TRTCLiveRoomCustomTitle_titleText);
            mCanBack = ta.getBoolean(R.styleable.TRTCLiveRoomCustomTitle_canBack, true);
            mBackText = ta.getString(R.styleable.TRTCLiveRoomCustomTitle_backText);
            mMoreText = ta.getString(R.styleable.TRTCLiveRoomCustomTitle_moreText);
            setUpView();
        } finally {
            ta.recycle();
        }
    }

    private void setUpView() {
        mTextBack = (TextView) findViewById(R.id.menu_return);
        mTextBack.setTextColor(getResources().getColor(R.color.trtcliveroom_color_white));
        mTextTitle = (TextView) findViewById(R.id.title);
        mTextMore = (TextView) findViewById(R.id.menu_more);


        if (!mCanBack) {
            mTextBack.setVisibility(View.GONE);
        }

        mTextBack.setText(mBackText);
        mTextMore.setText(mMoreText);
        mTextTitle.setText(mTitleText);
    }

    public void setTitle(String title) {
        mTitleText = title;
        mTextTitle.setText(title);
    }

    public void setMoreText(String title) {
        mMoreText = title;
        mTextMore.setText(title);
    }

    public void setReturnText(String strReturn) {
        mBackText = strReturn;
        mTextBack.setText(strReturn);
    }

    public void setReturnListener(OnClickListener listener) {
        mTextBack.setOnClickListener(listener);
    }

    public void setMoreListener(OnClickListener listener) {
        if (!TextUtils.isEmpty(mMoreText)) {
            mTextMore.setOnClickListener(listener);
        }
    }
}
