package com.tencent.qcloud.tuikit.tuigift.view;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;

import androidx.annotation.Nullable;

import com.tencent.qcloud.tuikit.tuigift.R;


/**
 * 礼物面板展示按钮
 * 点击此按钮弹出礼物面板
 */
public class TUIGiftButton extends FrameLayout {
    private String  mGroupId;
    private Context mContext;

    public TUIGiftButton(Context context) {
        super(context);
        this.mContext = context;
        initView(context);
    }

    public TUIGiftButton(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        this.mContext = context;
        initView(context);
    }

    public TUIGiftButton(Context context, String groupId) {
        this(context);
        this.mContext = context;
        this.mGroupId = groupId;
        initView(context);
    }

    public void initView(Context context) {
        View view = LayoutInflater.from(context).inflate(R.layout.tuigift_extension_view, this);
        final TUIGiftListPanelPlugView panelPlugView = new TUIGiftListPanelPlugView(mContext, mGroupId);
        findViewById(R.id.iv_show_panel).setOnClickListener(new OnClickListener() {
            @Override
            public void onClick(View v) {
                panelPlugView.show();
                panelPlugView.setListener();
            }
        });
    }

    @Override
    public void setBackground(Drawable background) {
        super.setBackground(background);
    }
}
