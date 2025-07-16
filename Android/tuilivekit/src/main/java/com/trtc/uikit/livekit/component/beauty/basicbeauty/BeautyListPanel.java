package com.trtc.uikit.livekit.component.beauty.basicbeauty;

import static com.trtc.uikit.livekit.component.beauty.basicbeauty.store.BasicBeautyState.ITEM_BEAUTY_CLOSE;
import static com.trtc.uikit.livekit.component.beauty.basicbeauty.store.BasicBeautyState.ITEM_BEAUTY_RUDDY;
import static com.trtc.uikit.livekit.component.beauty.basicbeauty.store.BasicBeautyState.ITEM_BEAUTY_SMOOTH;
import static com.trtc.uikit.livekit.component.beauty.basicbeauty.store.BasicBeautyState.ITEM_BEAUTY_WHITENESS;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Rect;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.SeekBar;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.beauty.basicbeauty.service.BasicBeautyService;
import com.trtc.uikit.livekit.component.beauty.basicbeauty.store.BasicBeautyStore;
import com.trtc.uikit.livekit.component.beauty.basicbeauty.store.BasicBeautyState;
import com.trtc.uikit.livekit.component.beauty.basicbeauty.view.BeautyListAdapter;

@SuppressLint("ViewConstructor")
public class BeautyListPanel extends FrameLayout {

    private BeautyListAdapter  mBeautyListAdapter;
    private SeekBar            mBeautySeekBar;
    private TextView           mTextBeautyLevel;
    private TextView           mTextBeautyType;
    private RecyclerView       mRecycleBeautyList;
    private BasicBeautyService mService;
    private BasicBeautyState   mState;

    private final Observer<Integer> mBeautyListener = this::obBeautyTypeChange;

    public BeautyListPanel(@NonNull Context context) {
        this(context, null);
    }

    public BeautyListPanel(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public BeautyListPanel(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);

        init();
    }

    private void init() {
        initService();
        initView();
    }

    private void initService() {
        mService = BasicBeautyStore.getInstance().basicBeautyService;
        mState = BasicBeautyStore.getInstance().beautyState;
    }

    private void initView() {
        LayoutInflater.from(getContext()).inflate(R.layout.beauty_basic_beauty_panel, this, true);
        mRecycleBeautyList = findViewById(R.id.rv_beauty_list);
        mTextBeautyType = findViewById(R.id.beauty_tv_seek_bar_type);
        mBeautySeekBar = findViewById(R.id.beauty_seek_bar);
        mTextBeautyLevel = findViewById(R.id.beauty_tv_seek_bar_level);

        initBeautyList();
        initBeautySeekBar();
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        mState.mCurrentBeautyType.observeForever(mBeautyListener);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        mState.mCurrentBeautyType.removeObserver(mBeautyListener);

    }

    public void closeBeauty() {
        setSeekBarVisibility(GONE);
        mService.closeBeauty();
    }

    public void enableSmooth() {
        final int currentProgress = mState.smoothLevel.getValue();
        setSeekBarVisibility(VISIBLE);
        mTextBeautyType.setText(R.string.common_beauty_item_smooth);
        mBeautySeekBar.setMax(9);
        mBeautySeekBar.setProgress(currentProgress);
        mTextBeautyLevel.setText(String.valueOf(mBeautySeekBar.getProgress()));
    }

    public void enableWhiteness() {
        final int currentProgress = mState.whitenessLevel.getValue();
        setSeekBarVisibility(VISIBLE);
        mTextBeautyType.setText(R.string.common_beauty_item_whiteness);
        mBeautySeekBar.setMax(9);
        mBeautySeekBar.setProgress(currentProgress);
        mTextBeautyLevel.setText(String.valueOf(mBeautySeekBar.getProgress()));
    }

    public void enableRuddy() {
        final int currentProgress = mState.ruddyLevel.getValue();
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
                switch (mState.mCurrentBeautyType.getValue()) {
                    case ITEM_BEAUTY_SMOOTH:
                        mService.setBeautyLevel(progress);
                        break;
                    case ITEM_BEAUTY_WHITENESS:
                        mService.setWhitenessLevel(progress);
                        break;
                    case ITEM_BEAUTY_RUDDY:
                        mService.setRuddyLevel(progress);
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
        mBeautyListAdapter = new BeautyListAdapter(getContext());
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
            case ITEM_BEAUTY_CLOSE:
                closeBeauty();
                break;
            case ITEM_BEAUTY_SMOOTH:
                enableSmooth();
                break;
            case ITEM_BEAUTY_WHITENESS:
                enableWhiteness();
                break;
            case ITEM_BEAUTY_RUDDY:
                enableRuddy();
                break;
            default:
                break;
        }
    }
}
