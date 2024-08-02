package com.tencent.effect.beautykit.view.panelview;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Typeface;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.URLUtil;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.bumptech.glide.Glide;
import com.tencent.effect.beautykit.R;
import com.tencent.effect.beautykit.config.TEUIConfig;
import com.tencent.effect.beautykit.model.TEUIProperty;
import com.tencent.effect.beautykit.utils.PanelDisplay;
import com.tencent.effect.beautykit.view.widget.PanelItemSelectorLayout;

import java.io.File;
import java.util.List;


class TEDetailPanelAdapter extends RecyclerView.Adapter<TEDetailPanelAdapter.UIPropertyHolder>
        implements View.OnClickListener {
    private ItemClickListener mListener;
    private List<TEUIProperty> uiPropertyList;
    private LinearLayoutManager layoutManager = null;
    private int selectedPosition = 0;


    private static final String ASSETS_NAME = "file:///android_asset/";
    private static final String HTTP_NAME = "http";

    private TEUIConfig uiConfig = TEUIConfig.getInstance();

    public TEDetailPanelAdapter(Context context, LinearLayoutManager layoutManager, ItemClickListener mListener) {
        this.mListener = mListener;
        this.layoutManager = layoutManager;
    }


    @NonNull
    @Override
    public UIPropertyHolder onCreateViewHolder(ViewGroup viewGroup, int type) {
        View view = LayoutInflater.from(viewGroup.getContext()).inflate(
                R.layout.te_beauty_panel_view_recycle_view_item_layout, viewGroup, false);
        return new UIPropertyHolder(view);
    }

    @Override
    public int getItemCount() {
        if (uiPropertyList == null) {
            return 0;
        }
        return uiPropertyList.size();
    }

    @SuppressLint("UseCompatLoadingForDrawables")
    @Override
    public void onBindViewHolder(UIPropertyHolder holder, int position) {
        TEUIProperty uiProperty = uiPropertyList.get(position);
        holder.tePanelItemRightDivider.setVisibility(View.GONE);
        holder.itemView.setTag(position);
        holder.tePanelItemLabel.setText(PanelDisplay.getDisplayName(uiProperty));
        holder.tePanelItemIcon.setVisibility(View.VISIBLE);
        if (!TextUtils.isEmpty(uiProperty.icon)) {
            loadImage(uiProperty, holder.tePanelItemIcon);
        }
        holder.itemView.setOnClickListener(this);
        if (uiProperty.getUiState() == TEUIProperty.UIState.CHECKED_AND_IN_USE) {
            holder.tePanelItemLabel.setTextColor(this.uiConfig.textCheckedColor);
            holder.tePanelItemLabel.setTypeface(null, Typeface.BOLD);
            if (holder.bgLayout.getCheckedColor() != this.uiConfig.panelItemCheckedColor) {
                holder.bgLayout.setCheckedColor(this.uiConfig.panelItemCheckedColor);
            }
            holder.bgLayout.setChecked(true);
        } else {
            holder.tePanelItemLabel.setTextColor(this.uiConfig.textColor);
            holder.tePanelItemLabel.setTypeface(null, Typeface.NORMAL);
            holder.bgLayout.setChecked(false);
        }
        if (uiProperty.isNoneItem()) {
            holder.tePanelItemRightDivider.setBackgroundColor(this.uiConfig.panelDividerColor);
            holder.tePanelItemRightDivider.setVisibility(View.VISIBLE);
        } else {
            holder.tePanelItemRightDivider.setVisibility(View.GONE);
        }
        if (isShowPoint(uiProperty)) {
            holder.tePanelItemPointView.setBackgroundColor(this.uiConfig.panelItemCheckedColor);
            holder.tePanelItemPointView.setVisibility(View.VISIBLE);
        } else {
            holder.tePanelItemPointView.setVisibility(View.GONE);
        }
    }

    private void loadImage(TEUIProperty uiProperty, ImageView imageView) {
        if (uiProperty.icon.startsWith(HTTP_NAME) || uiProperty.icon.startsWith("/")) {
            Glide.with(imageView).load(URLUtil.isNetworkUrl(uiProperty.icon) ? uiProperty.icon :
                    new File(uiProperty.icon)).into(imageView);
        } else {
            Glide.with(imageView).load(ASSETS_NAME + uiProperty.icon).into(imageView);
        }
    }


    private boolean isShowPoint(TEUIProperty uiProperty) {
        if (uiProperty.uiCategory != TEUIProperty.UICategory.BEAUTY) {
            return false;
        }
        if (uiProperty.getUiState() != TEUIProperty.UIState.INIT
                && uiProperty.sdkParam != null
                && uiProperty.sdkParam.effectValue != 0) {
            return true;
        } else if (uiProperty.propertyList != null) {
            for (TEUIProperty ui : uiProperty.propertyList) {
                if (isShowPoint(ui)) {
                    return true;
                }
            }
            return false;
        }
        return false;
    }

    @Override
    public void onClick(View v) {
        if (mListener != null) {
            selectedPosition = (int) v.getTag();
            TEUIProperty uiProperty = this.uiPropertyList.get(selectedPosition);
            mListener.onItemClick(uiProperty);
        }
    }

    public int findFirstVisibleItemPosition() {
        if (layoutManager != null) {
            return layoutManager.findFirstVisibleItemPosition();
        }
        return 0;
    }

    public void setProperties(List<TEUIProperty> properties) {
        this.uiPropertyList = properties;
        notifyDataSetChanged();
    }



    public void updateUIConfig(TEUIConfig uiConfig) {
        this.uiConfig = uiConfig;
        this.notifyDataSetChanged();
    }



    public void scrollToPosition(int position) {
        layoutManager.scrollToPositionWithOffset(position, 0);
    }


    public void refreshCurrentItemState() {
        this.notifyItemChanged(selectedPosition);
    }

    public interface ItemClickListener {
        void onItemClick(TEUIProperty uiProperty);
    }

    public boolean isCheckedBeautyCloseItem() {
        if (this.uiPropertyList == null) {
            return false;
        }
        TEUIProperty uiProperty = this.uiPropertyList.get(selectedPosition);
        return uiProperty.isNoneItem() && uiProperty.getUiState() == TEUIProperty.UIState.CHECKED_AND_IN_USE;
    }


    static class UIPropertyHolder extends RecyclerView.ViewHolder {
        TextView tePanelItemLabel;
        ImageView tePanelItemIcon;
        View itemView;

        View tePanelItemRightDivider;

        PanelItemSelectorLayout bgLayout = null;

        View tePanelItemPointView;

        public UIPropertyHolder(View itemView) {
            super(itemView);
            this.itemView = itemView;
            bgLayout = itemView.findViewById(R.id.te_beauty_panel_view_item_left_layout);
            tePanelItemLabel = itemView.findViewById(R.id.te_panel_view_item_layout_name);
            tePanelItemIcon = itemView.findViewById(R.id.te_panel_view_item_layout_icon);
            tePanelItemRightDivider = itemView.findViewById(R.id.te_panel_view_item_layout_right_divider);
            tePanelItemPointView = itemView.findViewById(R.id.te_panel_view_item_layout_point_view);
        }
    }


}
