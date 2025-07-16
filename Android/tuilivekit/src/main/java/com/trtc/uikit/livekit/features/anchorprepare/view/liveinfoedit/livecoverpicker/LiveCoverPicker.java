package com.trtc.uikit.livekit.features.anchorprepare.view.liveinfoedit.livecoverpicker;

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

public class LiveCoverPicker extends PopupDialog {
    private static final int COVER_IMAGE_DEFAULT_WIDTH = 114;

    private final Config              mConfig;
    private       String              mSelectedImageURL;
    private       OnItemClickListener mOnItemClickListener;

    public LiveCoverPicker(Context context, Config config) {
        super(context);
        mConfig = config;
        initView(context);
    }

    public void setOnItemClickListener(OnItemClickListener listener) {
        mOnItemClickListener = listener;
    }

    private void initView(Context context) {
        View view = View.inflate(getContext(), R.layout.anchor_prepare_layout_select_cover, null);

        initTitleView(view);
        initCoverImagePickRecyclerView(view);
        initSetCoverButton(view);

        setView(view);
    }

    private void initTitleView(View view) {
        TextView title = view.findViewById(R.id.title);
        title.setText(mConfig.title);

        view.findViewById(R.id.iv_back).setOnClickListener(v -> dismiss());
    }

    private void initCoverImagePickRecyclerView(View view) {
        RecyclerView recyclerView = view.findViewById(R.id.rv_image);
        recyclerView.setBackgroundResource(R.drawable.anchor_prepare_dialog_background);
        setView(recyclerView);
        int spanCount = calculateViewColumnCount(getContext());
        recyclerView.setLayoutManager(new GridLayoutManager(getContext(), spanCount));
        recyclerView.addItemDecoration(new LiveCoverImagePcikAdapter.GridDividerItemDecoration(getContext()));
        int selectedPosition = mConfig.data.indexOf(mConfig.currentImageUrl);
        mSelectedImageURL = mConfig.currentImageUrl;
        recyclerView.setAdapter(new LiveCoverImagePcikAdapter(getContext(), mConfig.data, selectedPosition, imageUrl
                -> mSelectedImageURL = imageUrl));
    }

    private void initSetCoverButton(View view) {
        TextView confirmButton = view.findViewById(R.id.btn_confirm);
        confirmButton.setText(mConfig.confirmButtonText);

        confirmButton.setOnClickListener(v -> {
            if (mOnItemClickListener != null) {
                mOnItemClickListener.onClick(mSelectedImageURL);
            }
            dismiss();
        });
    }

    private int calculateViewColumnCount(Context context) {
        DisplayMetrics metrics = context.getResources().getDisplayMetrics();
        int screenWidth = metrics.widthPixels;
        int itemWidth = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, COVER_IMAGE_DEFAULT_WIDTH,
                metrics);
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
