package com.trtc.uikit.livekit.livestream.view.widgets.beauty;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Rect;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.SeekBar;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.view.BasicView;

@SuppressLint("ViewConstructor")
public class BeautyListPanel extends BasicView {

    private BeautyListAdapter mBeautyListAdapter;
    private SeekBar           mBeautySeekBar;
    private TextView          mTextBeautyLevel;
    private TextView          mTextBeautyType;
    private RecyclerView      mRecycleBeautyList;

    private final Observer<Integer> mBeautyListener = this::obBeautyTypeChange;

    public BeautyListPanel(@NonNull Context context) {
        this(context, null);
    }

    public BeautyListPanel(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public BeautyListPanel(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_beauty_panel, this, true);
        mRecycleBeautyList = findViewById(R.id.rv_beauty_list);
        mTextBeautyType = findViewById(R.id.beauty_tv_seek_bar_type);
        mBeautySeekBar = findViewById(R.id.beauty_seek_bar);
        mTextBeautyLevel = findViewById(R.id.beauty_tv_seek_bar_level);
    }

    @Override
    protected void refreshView() {
        initBeautyList();
        initBeautySeekBar();
    }

    @Override
    protected void addObserver() {
        mBeautyListAdapter.mCurrentBeautyType.observeForever(mBeautyListener);
    }

    @Override
    protected void removeObserver() {
        mBeautyListAdapter.mCurrentBeautyType.removeObserver(mBeautyListener);
    }

    public void closeBeauty() {
        setSeekBarVisibility(GONE);
        mLiveManager.getMediaManager().closeBeauty();
    }

    public void enableSmooth() {
        final int currentProgress = mBeautyState.smoothLevel.getValue();
        setSeekBarVisibility(VISIBLE);
        mTextBeautyType.setText(R.string.common_beauty_item_smooth);
        mBeautySeekBar.setMax(9);
        mBeautySeekBar.setProgress(currentProgress);
        mTextBeautyLevel.setText(String.valueOf(mBeautySeekBar.getProgress()));
    }

    public void enableWhiteness() {
        final int currentProgress = mBeautyState.whitenessLevel.getValue();
        setSeekBarVisibility(VISIBLE);
        mTextBeautyType.setText(R.string.common_beauty_item_whiteness);
        mBeautySeekBar.setMax(9);
        mBeautySeekBar.setProgress(currentProgress);
        mTextBeautyLevel.setText(String.valueOf(mBeautySeekBar.getProgress()));
    }

    public void enableRuddy() {
        final int currentProgress = mBeautyState.ruddyLevel.getValue();
        setSeekBarVisibility(VISIBLE);
        mTextBeautyType.setText(R.string.common_beauty_item_ruddy);
        mBeautySeekBar.setMax(9);
        mBeautySeekBar.setProgress(currentProgress);
        mTextBeautyLevel.setText(String.valueOf(mBeautySeekBar.getProgress()));
    }

    private void initBeautySeekBar() {
        mBeautySeekBar.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            @Override
            public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
                mTextBeautyLevel.setText(String.valueOf(progress));
                switch (mBeautyListAdapter.mCurrentBeautyType.getValue()) {
                    case BeautyListAdapter.ITEM_BEAUTY_SMOOTH:
                        mMediaManager.setBeautyLevel(progress);
                        break;
                    case BeautyListAdapter.ITEM_BEAUTY_WHITENESS:
                        mMediaManager.setWhitenessLevel(progress);
                        break;
                    case BeautyListAdapter.ITEM_BEAUTY_RUDDY:
                        mMediaManager.setRuddyLevel(progress);
                        break;
                    default:
                        break;
                }
            }

            @Override
            public void onStartTrackingTouch(SeekBar seekBar) {

            }

            @Override
            public void onStopTrackingTouch(SeekBar seekBar) {

            }
        });
    }

    private void initBeautyList() {
        mBeautyListAdapter = new BeautyListAdapter(mContext);
        final int spanCount = mBeautyListAdapter.getItemCount();
        mRecycleBeautyList.setLayoutManager(new GridLayoutManager(getContext(), spanCount));
        int screenWidth = ScreenUtil.getScreenWidth(getContext());
        int itemWidth = ScreenUtil.dip2px(56);
        int spanSpace0 = (screenWidth - spanCount * itemWidth) / (spanCount);
        int spanSpace1 = (screenWidth - spanCount * itemWidth) / (spanCount + 1);
        mRecycleBeautyList.addItemDecoration(new RecyclerView.ItemDecoration() {
            @Override
            public void getItemOffsets(@NonNull Rect outRect, @NonNull View view,
                                       @NonNull RecyclerView parent, @NonNull RecyclerView.State state) {
                int position = parent.getChildLayoutPosition(view) % spanCount;
                outRect.left = (1 + position) * spanSpace1 - position * spanSpace0;
            }
        });
        mRecycleBeautyList.setAdapter(mBeautyListAdapter);
    }

    private void setSeekBarVisibility(int visibility) {
        mBeautySeekBar.setVisibility(visibility);
        mTextBeautyType.setVisibility(visibility);
        mTextBeautyLevel.setVisibility(visibility);
    }


    private void obBeautyTypeChange(Integer type) {
        switch (type) {
            case BeautyListAdapter.ITEM_BEAUTY_CLOSE:
                closeBeauty();
                break;
            case BeautyListAdapter.ITEM_BEAUTY_SMOOTH:
                enableSmooth();
                break;
            case BeautyListAdapter.ITEM_BEAUTY_WHITENESS:
                enableWhiteness();
                break;
            case BeautyListAdapter.ITEM_BEAUTY_RUDDY:
                enableRuddy();
                break;
            default:
                break;
        }
    }
}
