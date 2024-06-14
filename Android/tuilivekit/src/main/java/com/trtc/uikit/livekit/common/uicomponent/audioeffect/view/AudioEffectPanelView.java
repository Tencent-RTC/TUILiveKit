package com.trtc.uikit.livekit.common.uicomponent.audioeffect.view;


import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.SeekBar;
import android.widget.TextView;

import androidx.appcompat.widget.SwitchCompat;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.audioeffect.service.AudioEffectService;
import com.trtc.uikit.livekit.common.uicomponent.audioeffect.store.AudioEffectStore;
import com.trtc.uikit.livekit.common.view.BottomPanelView;
import com.trtc.uikit.livekit.manager.LiveController;

@SuppressLint("ViewConstructor")
public class AudioEffectPanelView extends BottomPanelView {

    private       TextView           mTextMusicVolume;
    private       TextView           mTextEarReturnVolume;
    private       TextView           mTextPeopleVolume;
    private final AudioEffectService mAudioEffectService;
    private final AudioEffectStore   mAudioEffectStore;

    public AudioEffectPanelView(Context context, LiveController liveController) {
        super(context, liveController);
        mAudioEffectService = new AudioEffectService(liveController);
        mAudioEffectStore = mAudioEffectService.mAudioEffectStore;
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_settings_audio_effect_panel, this,
                true);
        bindViewId();

        initBackView();
        initChangeVoiceView();
        initReverbView();
        initEarReturnVolumeView();
        initMusicVolumeView();
        initPeopleVolumeView();
        initEnableEarReturnView();
        initEarReturnVolume();
    }

    @Override
    protected void addObserver() {
    }

    @Override
    protected void removeObserver() {
    }

    private void bindViewId() {
        mTextMusicVolume = findViewById(R.id.tv_music_volume);
        mTextEarReturnVolume = findViewById(R.id.tv_ear_return_volume);
        mTextPeopleVolume = findViewById(R.id.tv_people_volume);
    }

    private void initEarReturnVolume() {
        SeekBar seekEarReturnVolume = findViewById(R.id.sb_ear_return_volume);
        seekEarReturnVolume.setProgress(mAudioEffectStore.earMonitorVolume.get());

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
                mAudioEffectService.setVoiceEarMonitorVolume(seekBar.getProgress());
            }
        });
    }

    private void initEnableEarReturnView() {
        SwitchCompat switchEarReturn = findViewById(R.id.sc_enable_ear);
        switchEarReturn.setChecked(mAudioEffectStore.enableVoiceEarMonitor.get());
        switchEarReturn.setOnCheckedChangeListener(
                (button, enable) -> mAudioEffectService.enableVoiceEarMonitor(enable));
    }

    private void initPeopleVolumeView() {
        mTextPeopleVolume.setText(String.valueOf(mAudioEffectStore.voiceVolume.get()));

        SeekBar seekPeopleVolume = findViewById(R.id.sb_people_volume);
        seekPeopleVolume.setProgress(mAudioEffectStore.voiceVolume.get());
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
                mAudioEffectService.setVoiceVolume(seekBar.getProgress());
            }
        });
    }

    private void initMusicVolumeView() {
        mTextMusicVolume.setText((String.valueOf(mAudioEffectStore.musicVolume.get())));

        SeekBar seekMusicVolume = findViewById(R.id.sb_music_volume);
        seekMusicVolume.setProgress(mAudioEffectStore.musicVolume.get());
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
                mAudioEffectService.setMusicVolume(seekBar.getProgress());
            }
        });
    }

    private void initEarReturnVolumeView() {
        mTextEarReturnVolume.setText((String.valueOf(mAudioEffectStore.earMonitorVolume.get())));
    }

    private void initBackView() {
        findViewById(R.id.iv_back).setOnClickListener((view) -> onBackButtonClick());
    }

    private void initReverbView() {
        RecyclerView recyclerReverb = findViewById(R.id.rv_reverb);
        recyclerReverb.setLayoutManager(
                new LinearLayoutManager(mContext, LinearLayoutManager.HORIZONTAL, false));
        ReverbAdapter adapterReverb = new ReverbAdapter(mContext, mAudioEffectService);
        recyclerReverb.setAdapter(adapterReverb);
    }

    private void initChangeVoiceView() {
        RecyclerView recyclerChangeVoice = findViewById(R.id.rv_change_voice);
        ChangeVoiceAdapter adapterChangeVoice = new ChangeVoiceAdapter(mContext, mAudioEffectService);
        recyclerChangeVoice.setLayoutManager(
                new LinearLayoutManager(mContext, LinearLayoutManager.HORIZONTAL, false));
        recyclerChangeVoice.setAdapter(adapterChangeVoice);
    }
}
