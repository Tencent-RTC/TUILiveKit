package com.tencent.effect.beautykit.view.panelview;


import static com.tencent.effect.beautykit.view.widget.SwitchLayout.SWITCH_LEFT_CHECKED;
import static com.tencent.effect.beautykit.view.widget.SwitchLayout.SWITCH_RIGHT_CHECKED;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.Nullable;

import com.tencent.effect.beautykit.R;
import com.tencent.effect.beautykit.model.TEPanelMenuCategory;
import com.tencent.effect.beautykit.view.widget.SwitchLayout;



public class TEPanelMenuView extends FrameLayout implements View.OnClickListener {

    private TEPanelMenuViewListener mListener = null;
    private SwitchLayout switchLayout = null;


    public TEPanelMenuView(Context context) {
        this(context, null);
    }

    public TEPanelMenuView(Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public TEPanelMenuView(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        this.initViews(context);
    }


    private void initViews(Context context) {
        LayoutInflater.from(context).inflate(R.layout.te_beauty_panel_menu_view_layout, this, true);
        findViewById(R.id.te_panel_menu_layout_motion_item).setOnClickListener(this);
        findViewById(R.id.te_panel_menu_layout_beauty_item).setOnClickListener(this);
        findViewById(R.id.te_panel_menu_layout_camera_item).setOnClickListener(this);
        findViewById(R.id.te_panel_menu_layout_makeup_item).setOnClickListener(this);
        findViewById(R.id.te_panel_menu_layout_lut_item).setOnClickListener(this);
        this.switchLayout = findViewById(R.id.te_panel_menu_switch_layout);
        this.switchLayout.setText(getResources().getString(R.string.te_beauty_panel_view_expand_original_txt),
                getResources().getString(R.string.te_beauty_panel_view_expand_te_txt));
        this.switchLayout.check(SWITCH_RIGHT_CHECKED);
        this.switchLayout.setSwitchLayoutListener(text -> {
            if (mListener == null) {
                return;
            }
            mListener.onCloseEffect(SWITCH_LEFT_CHECKED == text);
        });

    }


    public void setListener(TEPanelMenuViewListener mListener) {
        this.mListener = mListener;
    }

    @SuppressLint("NonConstantResourceId")
    @Override
    public void onClick(View v) {
        if (mListener == null) {
            return;
        }
        int id = v.getId();
        if (id == R.id.te_panel_menu_layout_motion_item) {
            mListener.onPanelMenuItemClick(TEPanelMenuCategory.MOTION);
        } else if (id == R.id.te_panel_menu_layout_beauty_item) {
            mListener.onPanelMenuItemClick(TEPanelMenuCategory.BEAUTY);
        } else if (id == R.id.te_panel_menu_layout_camera_item) {
            mListener.onPanelMenuItemClick(TEPanelMenuCategory.CAMERA);
        } else if (id == R.id.te_panel_menu_layout_makeup_item) {
            mListener.onPanelMenuItemClick(TEPanelMenuCategory.MAKEUP);
        } else if (id == R.id.te_panel_menu_layout_lut_item) {
            mListener.onPanelMenuItemClick(TEPanelMenuCategory.LUT);
        }
    }


    public void setItemClickable(TEPanelMenuCategory panelDataType) {

        ImageView icon = null;
        TextView menuNameTxt = null;
        switch (panelDataType) {
            case BEAUTY:
                icon = findViewById(R.id.te_panel_menu_layout_beauty_item_icon);
                menuNameTxt = findViewById(R.id.te_panel_menu_layout_beauty_item_text);
                break;
            case MAKEUP:
                icon = findViewById(R.id.te_panel_menu_layout_makeup_item_icon);
                menuNameTxt = findViewById(R.id.te_panel_menu_layout_makeup_item_text);
                break;
            case MOTION:
                icon = findViewById(R.id.te_panel_menu_layout_motion_item_icon);
                menuNameTxt = findViewById(R.id.te_panel_menu_layout_motion_item_text);
                break;
            case LUT:
                icon = findViewById(R.id.te_panel_menu_layout_lut_item_icon);
                menuNameTxt = findViewById(R.id.te_panel_menu_layout_lut_item_text);
                break;
            default:
                break;
        }
        if (icon != null) {
            icon.setBackgroundResource(R.drawable.te_beauty_panel_menu_item_bg_enable);
        }
        if (menuNameTxt != null) {
            menuNameTxt.setTextColor(getResources().getColor(R.color.te_beauty_color_FFFFFFFF));
        }
    }


    public interface TEPanelMenuViewListener {
        void onPanelMenuItemClick(TEPanelMenuCategory panelDataType);

        void onCloseEffect(boolean isClose);
    }


}
