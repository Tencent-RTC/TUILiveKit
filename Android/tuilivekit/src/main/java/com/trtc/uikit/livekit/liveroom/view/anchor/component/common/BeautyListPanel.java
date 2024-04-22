package com.trtc.uikit.livekit.liveroom.view.anchor.component.common;

import android.annotation.SuppressLint;
import android.content.Context;
import android.widget.SeekBar;
import android.widget.TextView;

import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.view.audience.component.VideoSettingsAdapter;

@SuppressLint("ViewConstructor")
public class BeautyListPanel extends ConstraintLayout {

    private BeautyListAdapter mBeautyListAdapter;
    private SeekBar           mBeautySeekBar;
    private TextView          mTextBeautyLevel;
    private TextView          mTextBeautyType;

    private final LiveRoomInfo      mLiveRoomInfo;
    private final RoomEngineService mRoomEngineService;
    private final Context           mContext;
    private final Observer<Integer> mBeautyListener = (type) -> {
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
    };

    public BeautyListPanel(Context context, LiveRoomInfo roomInfo, RoomEngineService service) {
        super(context);
        mContext = context;
        mLiveRoomInfo = roomInfo;
        mRoomEngineService = service;
        initView();
    }

    public void initView() {
        inflate(mContext, R.layout.livekit_anchor_beauty_panel, this);

        RecyclerView recycleBeautyList = findViewById(R.id.rv_beauty_list);
        recycleBeautyList.setLayoutManager(new GridLayoutManager(mContext, 5));
        mBeautyListAdapter = new BeautyListAdapter(mContext);
        recycleBeautyList.setAdapter(mBeautyListAdapter);

        mTextBeautyType = findViewById(R.id.beauty_tv_seek_bar_type);
        mBeautySeekBar = findViewById(R.id.beauty_seek_bar);
        mTextBeautyLevel = findViewById(R.id.beauty_tv_seek_bar_level);

        mBeautySeekBar.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            @Override
            public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
                mTextBeautyLevel.setText(String.valueOf(progress));
                switch (mBeautyListAdapter.mCurrentBeautyType.get()) {
                    case VideoSettingsAdapter.ITEM_BEAUTY_SMOOTH:
                        mLiveRoomInfo.anchorInfo.beautyInfo.smoothLevel.set(progress);
                        mRoomEngineService.setBeautyLevel(progress);
                        break;
                    case VideoSettingsAdapter.ITEM_BEAUTY_WHITENESS:
                        mLiveRoomInfo.anchorInfo.beautyInfo.whitenessLevel.set(progress);
                        mRoomEngineService.setWhitenessLevel(progress);
                        break;
                    case VideoSettingsAdapter.ITEM_BEAUTY_RUDDY:
                        mLiveRoomInfo.anchorInfo.beautyInfo.ruddyLevel.set(progress);
                        mRoomEngineService.setRuddyLevel(progress);
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

    public void closeBeauty() {
        setSeekBarVisibility(GONE);
        mLiveRoomInfo.anchorInfo.beautyInfo.smoothLevel.set(0);
        mRoomEngineService.setBeautyLevel(0);
        mLiveRoomInfo.anchorInfo.beautyInfo.whitenessLevel.set(0);
        mRoomEngineService.setWhitenessLevel(0);
        mLiveRoomInfo.anchorInfo.beautyInfo.ruddyLevel.set(0);
        mRoomEngineService.setRuddyLevel(0);
    }

    public void enableSmooth() {
        final int currentProgress = mLiveRoomInfo.anchorInfo.beautyInfo.smoothLevel.get();
        setSeekBarVisibility(VISIBLE);
        mTextBeautyType.setText(R.string.livekit_beauty_item_smooth);
        mBeautySeekBar.setMax(9);
        mBeautySeekBar.setProgress(currentProgress);
        mTextBeautyLevel.setText(String.valueOf(mBeautySeekBar.getProgress()));
    }

    public void enableWhiteness() {
        final int currentProgress = mLiveRoomInfo.anchorInfo.beautyInfo.whitenessLevel.get();
        setSeekBarVisibility(VISIBLE);
        mTextBeautyType.setText(R.string.livekit_beauty_item_whiteness);
        mBeautySeekBar.setMax(9);
        mBeautySeekBar.setProgress(currentProgress);
        mTextBeautyLevel.setText(String.valueOf(mBeautySeekBar.getProgress()));
    }

    public void enableRuddy() {
        final int currentProgress = mLiveRoomInfo.anchorInfo.beautyInfo.ruddyLevel.get();
        setSeekBarVisibility(VISIBLE);
        mTextBeautyType.setText(R.string.livekit_beauty_item_ruddy);
        mBeautySeekBar.setMax(9);
        mBeautySeekBar.setProgress(currentProgress);
        mTextBeautyLevel.setText(String.valueOf(mBeautySeekBar.getProgress()));
    }

    private void setSeekBarVisibility(int visibility) {
        mBeautySeekBar.setVisibility(visibility);
        mTextBeautyType.setVisibility(visibility);
        mTextBeautyLevel.setVisibility(visibility);
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        addObserver();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void addObserver() {
        mBeautyListAdapter.mCurrentBeautyType.observe(mBeautyListener);
    }

    private void removeObserver() {
        mBeautyListAdapter.mCurrentBeautyType.removeObserver(mBeautyListener);
    }
}
