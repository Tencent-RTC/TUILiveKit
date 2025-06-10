package com.trtc.uikit.component.barrage.view;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.drawable.Drawable;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.cardview.widget.CardView;
import androidx.core.content.res.ResourcesCompat;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.uikit.component.barrage.R;

import java.util.ArrayList;
import java.util.List;

@SuppressLint("ViewConstructor")
public class EmojiLayout extends FrameLayout {

    private final Context      context;
    private final RecyclerView emojiView;
    private final CardView     deleteView;

    private final int emojiSpace;

    private       EmojiListener emojiListener;
    private final List<Integer> emojiResIds = new ArrayList<>();

    public EmojiLayout(@NonNull Context context, List<Integer> emojiResIds) {
        super(context);
        inflate(context, R.layout.livekit_barrage_emoji_view, this);
        this.context = context;
        this.emojiResIds.addAll(emojiResIds);
        this.emojiSpace = ScreenUtil.dip2px(13);
        emojiView = findViewById(R.id.rv_emoji_list);
        deleteView = findViewById(R.id.cd_delete);
        initViews();
    }

    private void initViews() {
        int padding = emojiSpace / 2;
        emojiView.setPadding(padding, 0, padding, 0);
        int emojiSpanCount = 8;
        GridLayoutManager gridLayoutManager = new GridLayoutManager(context, emojiSpanCount);
        emojiView.setLayoutManager(gridLayoutManager);
        emojiView.setAdapter(new RecyclerView.Adapter<ImageViewHolder>() {
            @NonNull
            @Override
            public ImageViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
                int parentWidth = ((ViewGroup) emojiView.getParent()).getWidth();
                int itemSize = (parentWidth - 2 * (emojiSpanCount + 1) * padding) / emojiSpanCount + 2 * padding;
                ImageView imageView = new ImageView(context);
                ViewGroup.LayoutParams params = new LayoutParams(itemSize, itemSize);
                imageView.setLayoutParams(params);
                return new ImageViewHolder(imageView, padding);
            }

            @Override
            public void onBindViewHolder(@NonNull ImageViewHolder holder, int position) {
                int resId = emojiResIds.get(position);
                holder.imageView.setOnClickListener(v -> {
                    if (emojiListener != null) {
                        emojiListener.onAddEmoji(resId);
                    }
                });
                holder.bind(resId);
            }

            @Override
            public int getItemCount() {
                return emojiResIds.size();
            }
        });
        deleteView.setOnClickListener(v -> {
            if (emojiListener != null) {
                emojiListener.onDelete();
            }
        });
    }

    public void setDeleteViewEnable(boolean enable) {
        deleteView.getChildAt(0).setEnabled(enable);
    }

    public void setEmojiListener(EmojiListener listener) {
        emojiListener = listener;
    }

    private static class ImageViewHolder extends RecyclerView.ViewHolder {
        private final ImageView imageView;
        private final int       padding;

        public ImageViewHolder(@NonNull View itemView, int padding) {
            super(itemView);
            this.imageView = (ImageView) itemView;
            this.padding = padding;
        }

        void bind(int resId) {
            Drawable drawable = ResourcesCompat.getDrawable(itemView.getResources(), resId, null);
            imageView.setPadding(padding, padding, padding, padding);
            imageView.setImageDrawable(drawable);
        }
    }

    public interface EmojiListener {
        void onDelete();

        void onAddEmoji(int resId);
    }

}
