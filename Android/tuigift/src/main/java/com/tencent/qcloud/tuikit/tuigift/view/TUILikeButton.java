package com.tencent.qcloud.tuikit.tuigift.view;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;

import com.tencent.qcloud.tuikit.tuigift.R;
import com.tencent.qcloud.tuikit.tuigift.core.TUIGiftExtension;


/**
 * 点赞按钮
 */
public class TUILikeButton extends FrameLayout {
    private String  mGroupId;
    private Context mContext;

    public TUILikeButton(Context context) {
        super(context);
        this.mContext = context;
        initView(context);
    }

    public TUILikeButton(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        this.mContext = context;
        initView(context);
    }

    public TUILikeButton(Context context, String groupId) {
        this(context);
        this.mContext = context;
        this.mGroupId = groupId;
        initView(context);
    }

    public void initView(Context context) {
        View view = LayoutInflater.from(context).inflate(R.layout.tuigift_like_button, this);
        findViewById(R.id.iv_like).setOnClickListener(new OnClickListener() {
            @Override
            public void onClick(View v) {
                if (TUIGiftExtension.map.get(mGroupId + TUIGiftExtension.KEY_TYPE_PLUG) != null) {
                    ((TUIGiftListPanelPlugView) TUIGiftExtension.map
                            .get(mGroupId + TUIGiftExtension.KEY_TYPE_PLUG).get()).setListener();
                    ((TUIGiftListPanelView) TUIGiftExtension.map
                            .get(mGroupId + TUIGiftExtension.KEY_TYPE_PANEL).get()).sendLike();
                }
            }
        });
    }

    @Override
    public void setBackground(Drawable background) {
        super.setBackground(background);
    }
}
