package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.common;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.SeekBar;
import android.widget.TextView;

import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.manager.controller.MediaController;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.state.operation.BeautyState;
import com.trtc.uikit.livekit.view.liveroom.view.audience.component.livestreaming.link.VideoSettingsAdapter;

@SuppressLint("ViewConstructor")
public class BeautyListPanel extends BasicView {

    private BeautyListAdapter mBeautyListAdapter;
    private SeekBar           mBeautySeekBar;
    private TextView          mTextBeautyLevel;
    private TextView          mTextBeautyType;

    private final Observer<Integer> mBeautyListener = this::obBeautyTypeChange;

    public BeautyListPanel(Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_beauty_panel, this, true);
        bindViewId();
        
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
                MediaController mediaController = mLiveController.getMediaController();
                mTextBeautyLevel.setText(String.valueOf(progress));
                switch (mBeautyListAdapter.mCurrentBeautyType.get()) {
                    case VideoSettingsAdapter.ITEM_BEAUTY_SMOOTH:
                        mediaController.setBeautyLevel(progress);
                        break;
                    case VideoSettingsAdapter.ITEM_BEAUTY_WHITENESS:
                        mediaController.setWhitenessLevel(progress);
                        break;
                    case VideoSettingsAdapter.ITEM_BEAUTY_RUDDY:
                        mediaController.setRuddyLevel(progress);
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

    private void bindViewId() {
    }


    @Override
    protected void addObserver() {
        mBeautyListAdapter.mCurrentBeautyType.observe(mBeautyListener);
    }

    @Override
    protected void removeObserver() {
        mBeautyListAdapter.mCurrentBeautyType.removeObserver(mBeautyListener);
    }

    public void closeBeauty() {
        setSeekBarVisibility(GONE);
        mLiveController.getMediaController().closeBeauty();
    }

    public void enableSmooth() {
        final int currentProgress = mBeautyState.smoothLevel.get();
        setSeekBarVisibility(VISIBLE);
        mTextBeautyType.setText(R.string.livekit_beauty_item_smooth);
        mBeautySeekBar.setMax(9);
        mBeautySeekBar.setProgress(currentProgress);
        mTextBeautyLevel.setText(String.valueOf(mBeautySeekBar.getProgress()));
    }

    public void enableWhiteness() {
        final int currentProgress = mBeautyState.whitenessLevel.get();
        setSeekBarVisibility(VISIBLE);
        mTextBeautyType.setText(R.string.livekit_beauty_item_whiteness);
        mBeautySeekBar.setMax(9);
        mBeautySeekBar.setProgress(currentProgress);
        mTextBeautyLevel.setText(String.valueOf(mBeautySeekBar.getProgress()));
    }

    public void enableRuddy() {
        final int currentProgress = mBeautyState.ruddyLevel.get();
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
