package com.trtc.uikit.livekit.common.uicomponent.barrage.view.adapter;

import static android.text.Spanned.SPAN_EXCLUSIVE_EXCLUSIVE;

import android.content.Context;
import android.graphics.Paint;
import android.graphics.Rect;
import android.graphics.drawable.Drawable;
import android.text.SpannableStringBuilder;
import android.text.TextUtils;
import android.text.style.ForegroundColorSpan;
import android.text.style.ImageSpan;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.recyclerview.widget.RecyclerView;

import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.barrage.model.TUIBarrage;
import com.trtc.uikit.livekit.common.uicomponent.barrage.service.IEmojiResource;
import com.trtc.uikit.livekit.common.uicomponent.barrage.store.BarrageStore;
import com.trtc.uikit.livekit.common.uicomponent.barrage.view.util.RoundedImageSpan;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class BarrageMsgListAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {

    private final Context                mContext;
    private final List<TUIBarrage>       mMsgEntityList;
    private final OnItemClickListener    mOnItemClickListener;
    private final LayoutInflater         mLayoutInflater;
    private final IEmojiResource         mEmojiResource;
    private TUIBarrageDisplayAdapter     mCustomAdapter;

    public BarrageMsgListAdapter(Context context, List<TUIBarrage> msgEntityList,
                                 OnItemClickListener onItemClickListener) {
        this.mContext = context;
        this.mMsgEntityList = msgEntityList;
        this.mOnItemClickListener = onItemClickListener;
        this.mEmojiResource = BarrageStore.sharedInstance().mEmojiResource;
        this.mLayoutInflater = LayoutInflater.from(context);
    }

    public void setCustomAdapter(TUIBarrageDisplayAdapter adapter) {
        mCustomAdapter = adapter;
    }

    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        if (mCustomAdapter != null && viewType != 0) {
            RecyclerView.ViewHolder holder = mCustomAdapter.onCreateViewHolder(parent, viewType);
            if (holder != null) {
                return holder;
            }
        }
        View view = mLayoutInflater.inflate(R.layout.livekit_barrage_item_msg, parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(RecyclerView.ViewHolder holder, int position) {
        TUIBarrage item = mMsgEntityList.get(position);
        if (holder instanceof ViewHolder) {
            ((ViewHolder) holder).bind(item, mOnItemClickListener);
        } else if (mCustomAdapter != null) {
            mCustomAdapter.onBindViewHolder(holder, item);
        }
    }

    @Override
    public int getItemCount() {
        return mMsgEntityList.size();
    }

    @Override
    public int getItemViewType(int position) {
        TUIBarrage item = mMsgEntityList.get(position);
        int viewType = super.getItemViewType(position);
        if (mCustomAdapter != null) {
            viewType = mCustomAdapter.getItemViewType(position, item);
        }
        return viewType;
    }

    public interface OnItemClickListener {
        void onAgreeClick(int position);
    }


    public class ViewHolder extends RecyclerView.ViewHolder {
        private TextView mTvMsgContent;
        private TextView mBtnMsgAgree;

        public ViewHolder(View itemView) {
            super(itemView);
            initView(itemView);
        }

        private void initView(View itemView) {
            mTvMsgContent = itemView.findViewById(R.id.tv_msg_content);
            mBtnMsgAgree = itemView.findViewById(R.id.btn_msg_agree);
        }

        public void bind(final TUIBarrage barrage, final OnItemClickListener listener) {
            String level = TextUtils.isEmpty(barrage.user.level) ? "LV.0" : "LV." + barrage.user.level;
            String userName = TextUtils.isEmpty(barrage.user.userName) ? barrage.user.userId : barrage.user.userName;
            userName = TextUtils.isEmpty(userName) ? "" : userName;
            String result = level + userName + ": " + barrage.content;

            SpannableStringBuilder builder = new SpannableStringBuilder(result);
            int userNameColor = mContext.getResources().getColor(R.color.livekit_barrage_user_name_color);
            ForegroundColorSpan foreSpan = new ForegroundColorSpan(userNameColor);
            builder.setSpan(foreSpan, level.length(), level.length() + userName.length() + 1, SPAN_EXCLUSIVE_EXCLUSIVE);

            int textSize = (int) mTvMsgContent.getTextSize();
            int levelBackColor = mContext.getResources().getColor(R.color.livekit_barrage_level_back_color);
            RoundedImageSpan levelSpan = new RoundedImageSpan(level, textSize,
                    levelBackColor, textSize, ScreenUtil.dip2px(6));
            builder.setSpan(levelSpan, 0, level.length(), SPAN_EXCLUSIVE_EXCLUSIVE);

            Paint.FontMetrics fontMetrics = mTvMsgContent.getPaint().getFontMetrics();
            int fontSize = (int) (Math.abs(fontMetrics.ascent) + Math.abs(fontMetrics.descent));
            Rect rect = new Rect(0, 0, fontSize, fontSize);
            processEmojiSpan(builder, mEmojiResource, rect);
            mTvMsgContent.setText(builder);

            mBtnMsgAgree.setOnClickListener(v -> {
                if (listener != null) {
                    listener.onAgreeClick(getLayoutPosition());
                }
            });
        }

        private void processEmojiSpan(SpannableStringBuilder sb, IEmojiResource emojiResource, Rect rect) {
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
                    ImageSpan imageSpan = new ImageSpan(emojiDrawable);
                    sb.setSpan(imageSpan, index, index + item.length(), SPAN_EXCLUSIVE_EXCLUSIVE);
                }
            }
        }
    }
}