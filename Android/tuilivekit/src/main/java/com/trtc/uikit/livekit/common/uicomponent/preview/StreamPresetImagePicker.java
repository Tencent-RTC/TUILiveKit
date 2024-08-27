package com.trtc.uikit.livekit.common.uicomponent.preview;

import android.content.Context;
import android.util.DisplayMetrics;
import android.util.TypedValue;
import android.view.View;
import android.widget.TextView;

import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;

import java.util.List;

public class StreamPresetImagePicker extends PopupDialog {
    private final Config mConfig;

    private String              mSelectedImageURL;
    private OnItemClickListener mOnItemClickListener;

    public StreamPresetImagePicker(Context context, Config config) {
        super(context);
        mConfig = config;
        initView(context);
    }

    public void setOnItemClickListener(OnItemClickListener listener) {
        mOnItemClickListener = listener;
    }

    private void initView(Context context) {
        View view = View.inflate(getContext(), R.layout.livekit_layout_select_preset_image, null);
        TextView title = view.findViewById(R.id.title);
        title.setText(mConfig.title);
        TextView confirmButton = view.findViewById(R.id.btn_confirm);
        confirmButton.setText(mConfig.confirmButtonText);
        initRecycleView(context, view);
        initBackButton(view);
        initSetCoverButton(view);
        setView(view);
    }

    private void initRecycleView(Context context, View view) {
        RecyclerView recyclerView = view.findViewById(R.id.rv_image);
        recyclerView.setBackgroundResource(R.drawable.livekit_dialog_background);
        setView(recyclerView);
        int spanCount = calculateViewColumnCount(context);
        recyclerView.setLayoutManager(new GridLayoutManager(context, spanCount));
        recyclerView.addItemDecoration(new PresetImageGridAdapter.GridDividerItemDecoration(context));
        int selectedPosition = mConfig.data.indexOf(mConfig.currentImageUrl);
        mSelectedImageURL = mConfig.currentImageUrl;
        recyclerView.setAdapter(new PresetImageGridAdapter(context, mConfig.data, selectedPosition, imageUrl
                -> mSelectedImageURL = imageUrl));
    }

    private void initBackButton(View view) {
        view.findViewById(R.id.iv_back).setOnClickListener(v -> dismiss());
    }

    private void initSetCoverButton(View view) {
        view.findViewById(R.id.btn_confirm).setOnClickListener(v -> {
            if (mOnItemClickListener != null) {
                mOnItemClickListener.onClick(mSelectedImageURL);
            }
            dismiss();
        });
    }

    private int calculateViewColumnCount(Context context) {
        DisplayMetrics metrics = context.getResources().getDisplayMetrics();
        int imageViewDP = 114;
        int screenWidth = metrics.widthPixels;
        int itemWidth = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, imageViewDP, metrics);
        return screenWidth / itemWidth;
    }

    public static class Config {
        public String       title;
        public String       confirmButtonText;
        public List<String> data;
        public String       currentImageUrl;
    }

    public interface OnItemClickListener {
        void onClick(String imageUrl);
    }
}
