package com.trtc.uikit.livekit.component.barrage.view.adapter;

import static android.text.Spanned.SPAN_EXCLUSIVE_EXCLUSIVE;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Paint;
import android.graphics.Rect;
import android.graphics.drawable.Drawable;
import android.text.SpannableStringBuilder;
import android.text.TextUtils;
import android.text.method.LinkMovementMethod;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.barrage.service.IEmojiResource;
import com.trtc.uikit.livekit.component.barrage.store.BarrageStore;
import com.trtc.uikit.livekit.component.barrage.store.model.Barrage;
import com.trtc.uikit.livekit.component.barrage.view.EmojiSpan;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class BarrageItemDefaultAdapter implements BarrageItemAdapter {

    private final Context        mContext;
    private final String         mOwnerId;
    private final LayoutInflater mLayoutInflater;
    private final IEmojiResource mEmojiResource;

    public BarrageItemDefaultAdapter(Context context, String ownerId) {
        this.mContext = context;
        this.mOwnerId = ownerId;
        this.mEmojiResource = BarrageStore.sharedInstance().mEmojiResource;
        this.mLayoutInflater = LayoutInflater.from(context);
    }

    @NonNull
    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(@NonNull ViewGroup parent) {
        View view = mLayoutInflater.inflate(R.layout.livekit_barrage_item_msg, parent, false);
        return new ViewHolder(view);
    }

    @SuppressLint("SetTextI18n")
    @Override
    public void onBindViewHolder(@NonNull RecyclerView.ViewHolder holder, int position, Barrage barrage) {
        final ViewHolder viewHolder = (ViewHolder) holder;
        int level = getLevel(barrage);
        viewHolder.imageLevel.setImageResource(getLevelDrawable(level));
        viewHolder.layoutLevelBackground.setBackgroundResource(getLevelBackground(level));
        viewHolder.textLevel.setText("" + level);
        int fontSize = getFontSize(viewHolder.textMsgContent);
        if (TextUtils.equals(mOwnerId, barrage.user.userId)) {
            viewHolder.textAnchorFlag.setVisibility(View.VISIBLE);
            String placeHolder = getSpacesStringByDP(viewHolder.textMsgContent);
            viewHolder.textMsgContent.setText(viewHolder.getMessageBuilder(barrage, fontSize, placeHolder));
            viewHolder.textMsgContent.setMovementMethod(LinkMovementMethod.getInstance());
        } else {
            viewHolder.textAnchorFlag.setVisibility(View.GONE);
            viewHolder.textMsgContent.setText(viewHolder.getMessageBuilder(barrage, fontSize, ""));
        }
    }

    private int getLevel(Barrage barrage) {
        try {
            int level = Integer.parseInt(barrage.user.level);
            return Math.max(level, 0);
        } catch (Exception e) {
            return 0;
        }
    }

    private int getLevelDrawable(int level) {
        if (level <= 30) {
            return R.drawable.livekit_ic_level1;
        } else if (level <= 60) {
            return R.drawable.livekit_ic_level2;
        } else if (level <= 90) {
            return R.drawable.livekit_ic_level3;
        } else {
            return R.drawable.livekit_ic_level4;
        }
    }

    private int getLevelBackground(int level) {
        if (level <= 30) {
            return R.drawable.livekit_barrage_bg_leve1;
        } else if (level <= 60) {
            return R.drawable.livekit_barrage_bg_leve2;
        } else if (level <= 90) {
            return R.drawable.livekit_barrage_bg_leve3;
        } else {
            return R.drawable.livekit_barrage_bg_leve4;
        }
    }

    private String getSpacesStringByDP(TextView textView) {
        float dpWidth42 = TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, 46,
                textView.getContext().getResources().getDisplayMetrics());
        Paint paint = textView.getPaint();
        float spacesWidth = paint.measureText(" ");
        int spacesCount = (int) (dpWidth42 / spacesWidth) + 1;
        StringBuilder buildText = new StringBuilder();
        for (int i = 0; i < spacesCount; i++) {
            buildText.append(" ");
        }
        return buildText.toString();
    }

    public int getFontSize(TextView textView) {
        Paint paint = new Paint();
        paint.setTextSize(textView.getTextSize());
        Paint.FontMetrics fm = paint.getFontMetrics();
        return (int) Math.ceil(fm.bottom - fm.top);
    }

    public class ViewHolder extends RecyclerView.ViewHolder {
        private TextView     textMsgContent;
        private TextView     textLevel;
        private ImageView    imageLevel;
        private TextView     textAnchorFlag;
        private LinearLayout layoutLevelBackground;

        public ViewHolder(View itemView) {
            super(itemView);
            initView(itemView);
        }

        private void initView(View itemView) {
            textMsgContent = itemView.findViewById(R.id.tv_msg_content);
            textLevel = itemView.findViewById(R.id.tv_level);
            imageLevel = itemView.findViewById(R.id.iv_level);
            textAnchorFlag = itemView.findViewById(R.id.tv_anchor_flag);
            layoutLevelBackground = itemView.findViewById(R.id.ll_level_background);
        }

        public SpannableStringBuilder getMessageBuilder(final Barrage barrage, final int fontSize,
                                                        final String placeHolder) {
            String userName = TextUtils.isEmpty(barrage.user.userName) ? barrage.user.userId : barrage.user.userName;
            userName = TextUtils.isEmpty(userName) ? "" : userName;
            String result = "";
            if (TextUtils.isEmpty(placeHolder)) {
                result = userName + ": " + barrage.content;
            } else {
                result = placeHolder + userName + ": " + barrage.content;
            }
            SpannableStringBuilder builder = new SpannableStringBuilder(result);
            Rect rect = new Rect(0, 0, fontSize, fontSize);
            processEmojiSpan(builder, mEmojiResource, rect, fontSize);
            return builder;
        }

        private void processEmojiSpan(SpannableStringBuilder sb, IEmojiResource emojiResource, Rect rect,
                                      int fontSize) {
            if (sb == null || emojiResource == null) {
                return;
            }
            String text = sb.toString();
            Pattern pattern = Pattern.compile(emojiResource.getEncodePattern());
            List<String> matches = new ArrayList<>();
            Matcher matcher = pattern.matcher(sb);
            while (matcher.find()) {
                matches.add(matcher.group());
            }
            for (String item : matches) {
                int resId = emojiResource.getResId(item);
                if (resId == 0) {
                    continue;
                }
                int fromIndex = 0;
                while (fromIndex < text.length()) {
                    int index = text.indexOf(item, fromIndex);
                    if (index == -1) {
                        break;
                    }
                    fromIndex = index + item.length();
                    Drawable emojiDrawable = emojiResource.getDrawable(mContext, resId, rect);
                    if (emojiDrawable == null) {
                        continue;
                    }
                    emojiDrawable.setBounds(0, 0, fontSize, fontSize);
                    EmojiSpan imageSpan = new EmojiSpan(emojiDrawable, ScreenUtil.dip2px(0));
                    sb.setSpan(imageSpan, index, index + item.length(), SPAN_EXCLUSIVE_EXCLUSIVE);
                }
            }
        }
    }
}
