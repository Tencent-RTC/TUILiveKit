package com.trtc.uikit.livekit.component.gift.view;

import static android.text.Spanned.SPAN_EXCLUSIVE_EXCLUSIVE;
import static com.trtc.uikit.livekit.component.gift.service.GiftConstants.GIFT_COUNT;
import static com.trtc.uikit.livekit.component.gift.service.GiftConstants.GIFT_ICON_URL;
import static com.trtc.uikit.livekit.component.gift.service.GiftConstants.GIFT_NAME;
import static com.trtc.uikit.livekit.component.gift.service.GiftConstants.GIFT_RECEIVER_USERNAME;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Color;
import android.graphics.Rect;
import android.graphics.drawable.ColorDrawable;
import android.graphics.drawable.Drawable;
import android.text.SpannableStringBuilder;
import android.text.TextUtils;
import android.text.style.ForegroundColorSpan;
import android.text.style.ImageSpan;
import android.util.Log;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.bumptech.glide.Glide;
import com.bumptech.glide.request.target.CustomTarget;
import com.bumptech.glide.request.transition.Transition;
import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.barrage.store.model.Barrage;
import com.trtc.uikit.livekit.component.barrage.view.adapter.BarrageItemAdapter;
import com.trtc.uikit.livekit.component.gift.service.GiftConstants;

import java.security.SecureRandom;

public final class GiftBarrageAdapter implements BarrageItemAdapter {

    private static final String   TAG              = "GiftBarrageAdapter";
    private final        Context  mContext;
    private final        Drawable mDefaultGiftIcon = new ColorDrawable(Color.DKGRAY);

    public GiftBarrageAdapter(Context context) {
        mContext = context;
        int giftIconSize = 18;
        Rect bounds = new Rect(0, 0, ScreenUtil.dip2px(giftIconSize), ScreenUtil.dip2px(giftIconSize));
        mDefaultGiftIcon.setBounds(bounds);
    }

    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(ViewGroup parent) {
        LinearLayout ll = new LinearLayout(mContext);
        ll.addView(new TextView(mContext));
        return new GiftViewHolder(ll, mDefaultGiftIcon);
    }

    @Override
    public void onBindViewHolder(RecyclerView.ViewHolder holder, int position, Barrage barrage) {
        GiftViewHolder viewHolder = (GiftViewHolder) holder;
        viewHolder.bind(barrage);
    }

    private static class GiftViewHolder extends RecyclerView.ViewHolder {

        private final TextView     textView;
        private final Context      context;
        private final Drawable     defaultGiftIcon;
        private final SecureRandom random = new SecureRandom();

        public GiftViewHolder(View itemView, Drawable defaultGiftIcon) {
            super(itemView);
            this.context = itemView.getContext();
            this.defaultGiftIcon = defaultGiftIcon;
            LinearLayout linearLayout = (LinearLayout) itemView;
            textView = (TextView) linearLayout.getChildAt(0);
            linearLayout.setPadding(0, ScreenUtil.dip2px(5), 0, ScreenUtil.dip2px(5));
            textView.setTextColor(Color.WHITE);
            textView.setTextSize(12);
            textView.setGravity(Gravity.START | Gravity.CENTER_VERTICAL);
            textView.setPadding(ScreenUtil.dip2px(5), ScreenUtil.dip2px(5),
                    ScreenUtil.dip2px(5), ScreenUtil.dip2px(5));
            textView.setBackgroundResource(R.drawable.livekit_barrage_bg_msg_item);
        }

        public void bind(Barrage barrage) {
            SpannableStringBuilder sb = new SpannableStringBuilder();
            String sender = barrage.user.userName;
            sender = TextUtils.isEmpty(sender) ? "" : sender;
            sb.append(sender);
            int userNameColor = context.getResources().getColor(R.color.livekit_barrage_user_name_color);
            ForegroundColorSpan senderSpan = new ForegroundColorSpan(userNameColor);
            sb.setSpan(senderSpan, 0, sender.length(), SPAN_EXCLUSIVE_EXCLUSIVE);
            String send = context.getString(R.string.livekit_sent);
            sb.append(send);
            String receiver = String.valueOf(barrage.extInfo.get(GIFT_RECEIVER_USERNAME));
            receiver = TextUtils.isEmpty(receiver) ? "" : receiver;
            sb.append(receiver);
            ForegroundColorSpan receiverSpan = new ForegroundColorSpan(senderSpan.getForegroundColor());
            sb.setSpan(receiverSpan, sb.length() - receiver.length(), sb.length(), SPAN_EXCLUSIVE_EXCLUSIVE);
            String giftName = String.valueOf(barrage.extInfo.get(GIFT_NAME));
            sb.append(" ").append(giftName);
            int giftNameColor = context.getResources().getColor(
                    GiftConstants.MESSAGE_COLOR[random.nextInt(GiftConstants.MESSAGE_COLOR.length)]);
            ForegroundColorSpan giftSpan = new ForegroundColorSpan(giftNameColor);
            sb.setSpan(giftSpan, sb.length() - giftName.length(), sb.length(), SPAN_EXCLUSIVE_EXCLUSIVE);
            String url = String.valueOf(barrage.extInfo.get(GIFT_ICON_URL));
            ImageSpan imageSpan = new ImageSpan(defaultGiftIcon);
            sb.append(" ");
            int start = sb.length() - 1;
            sb.setSpan(imageSpan, sb.length() - 1, sb.length(), SPAN_EXCLUSIVE_EXCLUSIVE);
            Glide.with(context)
                    .asBitmap()
                    .load(url)
                    .into(new CustomTarget<Bitmap>() {
                        @Override
                        public void onResourceReady(@NonNull Bitmap resource, Transition<? super Bitmap> transition) {
                            ImageSpan span = new ImageSpan(context, resource);
                            span.getDrawable().setBounds(defaultGiftIcon.getBounds());
                            sb.setSpan(span, start, start + 1, SPAN_EXCLUSIVE_EXCLUSIVE);
                            textView.setText(sb);
                        }

                        @Override
                        public void onLoadCleared(Drawable placeholder) {
                            Log.e(TAG, "glide load failed: " + url);
                        }
                    });
            String count = String.valueOf(barrage.extInfo.get(GIFT_COUNT));
            sb.append("x").append(count);
            textView.setText(sb);
        }
    }
}
