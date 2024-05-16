package com.trtc.uikit.livekit.common.uicomponent.audioeffect;


import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.SeekBar;
import android.widget.TextView;

import androidx.appcompat.widget.SwitchCompat;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveController;
import com.trtc.uikit.livekit.common.core.controller.MediaController;
import com.trtc.uikit.livekit.common.view.BottomPanelView;
import com.trtc.uikit.livekit.liveroom.data.AudioInfo;

@SuppressLint("ViewConstructor")
public class AudioEffectPanelView extends BottomPanelView {

    private       TextView        mTextMusicVolume;
    private       TextView        mTextEarReturnVolume;
    private       TextView        mTextPeopleVolume;
    private final MediaController mMediaController;

    public AudioEffectPanelView(Context context, LiveController liveController) {
        super(context, liveController);
        mMediaController = mLiveController.getMediaController();
    }

    @Override
    protected void addObserver() {
    }

    @Override
    protected void removeObserver() {
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_settings_audio_effect_panel, this,
                true);
        mTextMusicVolume = findViewById(R.id.tv_music_volume);
        mTextEarReturnVolume = findViewById(R.id.tv_ear_return_volume);
        mTextPeopleVolume = findViewById(R.id.tv_people_volume);
        RecyclerView recyclerChangeVoice = findViewById(R.id.rv_change_voice);
        ChangeVoiceAdapter adapterChangeVoice = new ChangeVoiceAdapter(mContext, mLiveController);
        recyclerChangeVoice.setLayoutManager(
                new LinearLayoutManager(mContext, LinearLayoutManager.HORIZONTAL, false));
        recyclerChangeVoice.setAdapter(adapterChangeVoice);

        RecyclerView recyclerReverb = findViewById(R.id.rv_reverb);
        recyclerReverb.setLayoutManager(
                new LinearLayoutManager(mContext, LinearLayoutManager.HORIZONTAL, false));
        ReverbAdapter adapterReverb = new ReverbAdapter(mContext, mLiveController);
        recyclerReverb.setAdapter(adapterReverb);

        findViewById(R.id.iv_back).setOnClickListener((view) -> onBackButtonClick());
        AudioInfo audioInfo = mMediaState.audioInfo;

        mTextEarReturnVolume.setText((String.valueOf(audioInfo.earMonitorVolume.get())));
        mTextMusicVolume.setText((String.valueOf(audioInfo.musicVolume.get())));
        mTextPeopleVolume.setText(String.valueOf(audioInfo.voiceVolume.get()));

        SwitchCompat switchEarReturn = findViewById(R.id.sc_enable_ear);
        switchEarReturn.setChecked(audioInfo.enableVoiceEarMonitor.get());
        switchEarReturn.setOnCheckedChangeListener((button, enable) -> mMediaController.enableVoiceEarMonitor(enable));

        SeekBar seekEarReturnVolume = findViewById(R.id.sb_ear_return_volume);
        seekEarReturnVolume.setProgress(audioInfo.earMonitorVolume.get());
        SeekBar seekPeopleVolume = findViewById(R.id.sb_people_volume);
        seekPeopleVolume.setProgress(audioInfo.voiceVolume.get());
        SeekBar seekMusicVolume = findViewById(R.id.sb_music_volume);
        seekMusicVolume.setProgress(audioInfo.musicVolume.get());
        seekMusicVolume.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            @Override
            public void onProgressChanged(SeekBar seekBar, int i, boolean b) {
                mTextMusicVolume.setText(String.valueOf(i));
            }

            @Override
            public void onStartTrackingTouch(SeekBar seekBar) {

            }

            @Override
            public void onStopTrackingTouch(SeekBar seekBar) {
                mMediaController.setMusicVolume(seekBar.getProgress());
            }
        });

        seekEarReturnVolume.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            @Override
            public void onProgressChanged(SeekBar seekBar, int i, boolean b) {
                mTextEarReturnVolume.setText(String.valueOf(i));
            }

            @Override
            public void onStartTrackingTouch(SeekBar seekBar) {

            }

            @Override
            public void onStopTrackingTouch(SeekBar seekBar) {
                mMediaController.setVoiceEarMonitorVolume(seekBar.getProgress());
            }
        });

        seekPeopleVolume.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            @Override
            public void onProgressChanged(SeekBar seekBar, int i, boolean b) {
                mTextPeopleVolume.setText(String.valueOf(i));
            }

            @Override
            public void onStartTrackingTouch(SeekBar seekBar) {

            }

            @Override
            public void onStopTrackingTouch(SeekBar seekBar) {
                mMediaController.setVoiceVolume(seekBar.getProgress());
            }
        });
    }
}
