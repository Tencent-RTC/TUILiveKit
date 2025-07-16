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
import android.text.style.ForegroundColorSpan;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.barrage.service.IEmojiResource;
import com.trtc.uikit.livekit.component.barrage.store.BarrageStore;
import com.trtc.uikit.livekit.component.barrage.store.model.Barrage;
import com.trtc.uikit.livekit.component.barrage.view.EmojiSpan;

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
        int fontSize = getFontSize(viewHolder.textMsgContent);
        if (TextUtils.equals(mOwnerId, barrage.user.userId)) {
            viewHolder.textAnchorFlag.setVisibility(View.VISIBLE);
            String placeHolder = getSpacesStringByDP(viewHolder.textMsgContent);
            viewHolder.textMsgContent.setText(viewHolder.getMessageBuilder(barrage, fontSize, placeHolder));
            viewHolder.textMsgContent.setTextColor(mContext.getResources().getColor(R.color.livekit_barrage_g8));
            viewHolder.textMsgContent.setMovementMethod(LinkMovementMethod.getInstance());
        } else {
            viewHolder.textAnchorFlag.setVisibility(View.GONE);
            viewHolder.textMsgContent.setText(viewHolder.getMessageBuilder(barrage, fontSize, ""));
            viewHolder.textMsgContent.setTextColor(mContext.getResources().getColor(R.color.livekit_barrage_user_name_color));
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
        private TextView textMsgContent;
        private TextView textAnchorFlag;

        public ViewHolder(View itemView) {
            super(itemView);
            initView(itemView);
        }

        private void initView(View itemView) {
            textMsgContent = itemView.findViewById(R.id.tv_msg_content);
            textAnchorFlag = itemView.findViewById(R.id.tv_anchor_flag);
        }

        public SpannableStringBuilder getMessageBuilder(final Barrage barrage, final int fontSize,
                                                        final String placeHolder) {
            String userName = TextUtils.isEmpty(barrage.user.userName) ? barrage.user.userId : barrage.user.userName;
            userName = TextUtils.isEmpty(userName) ? "" : userName;
            String userNameSplicing = "";
            SpannableStringBuilder builder = new SpannableStringBuilder();
            int userNameColor = mContext.getResources().getColor(R.color.livekit_barrage_user_name_color);
            if (TextUtils.isEmpty(placeHolder)) {
                userNameSplicing = userName + ": ";
            } else {
                userNameSplicing = placeHolder + userName + ": ";
            }
            builder.append(userNameSplicing);
            ForegroundColorSpan userNameSpan = new ForegroundColorSpan(userNameColor);
            builder.setSpan(userNameSpan, 0, userNameSplicing.length(), SPAN_EXCLUSIVE_EXCLUSIVE);

            builder.append(barrage.content);
            int contentColor = mContext.getResources().getColor(R.color.livekit_barrage_g8);
            ForegroundColorSpan contentSpan = new ForegroundColorSpan(contentColor);
            builder.setSpan(contentSpan, builder.length() - barrage.content.length(), builder.length(),
                    SPAN_EXCLUSIVE_EXCLUSIVE);

            Rect rect = new Rect(0, 0, fontSize, fontSize);
            processEmojiSpan(builder, mEmojiResource, rect, fontSize);
            return builder;
        }

        private void processEmojiSpan(SpannableStringBuilder sb, IEmojiResource emojiResource, Rect rect,
                                      int fontSize) {
            String text = sb.toString();
            int startIndex = 0;
            for (int i = 0; i < text.length(); i++) {
                if (text.charAt(i) == '[') {
                    int endIndex = text.indexOf(']', i);
                    if (endIndex != -1) {
                        String emojiKey = text.substring(i, endIndex + 1);
                        int resId = emojiResource.getResId(emojiKey);
                        if (resId != 0) {
                            Drawable emojiDrawable = emojiResource.getDrawable(mContext, resId, rect);
                            if (emojiDrawable == null) {
                                continue;
                            }
                            emojiDrawable.setBounds(0, 0, fontSize, fontSize);
                            EmojiSpan imageSpan = new EmojiSpan(emojiDrawable, 0);
                            sb.setSpan(imageSpan, startIndex, endIndex + 1, SPAN_EXCLUSIVE_EXCLUSIVE);
                        }
                        startIndex = endIndex + 1;
                        i = endIndex;
                    }
                } else {
                    startIndex++;
                }
            }
        }
    }
}
