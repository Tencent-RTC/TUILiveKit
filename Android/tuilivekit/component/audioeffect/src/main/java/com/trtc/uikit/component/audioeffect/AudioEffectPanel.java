package com.trtc.uikit.component.audioeffect;


import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.SeekBar;
import android.widget.TextView;

import androidx.appcompat.widget.SwitchCompat;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.uikit.component.audioeffect.service.AudioEffectService;
import com.trtc.uikit.component.audioeffect.store.AudioEffectState;
import com.trtc.uikit.component.audioeffect.view.ChangeVoiceAdapter;
import com.trtc.uikit.component.audioeffect.view.ReverbAdapter;
import com.trtc.uikit.component.common.DataReporter;

@SuppressLint("ViewConstructor")
public class AudioEffectPanel extends FrameLayout {

    private static final int LIVEKIT_METRICS_PANEL_SHOW_LIVE_ROOM_AUDIO_EFFECT  = 190017;
    private static final int LIVEKIT_METRICS_PANEL_SHOW_VOICE_ROOM_AUDIO_EFFECT = 191015;

    private final Context            mContext;
    private       TextView           mTextMusicVolume;
    private       TextView           mTextEarReturnVolume;
    private       TextView           mTextPeopleVolume;
    private       AudioEffectService mAudioEffectService;
    private       AudioEffectState   mAudioEffectState;

    protected OnBackButtonClickListener mOnBackButtonClickListener;

    public AudioEffectPanel(Context context) {
        this(context, null);
    }

    public AudioEffectPanel(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AudioEffectPanel(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_settings_audio_effect_panel, this, true);
    }

    public void init(String roomId) {
        mAudioEffectService = new AudioEffectService(roomId);
        mAudioEffectState = mAudioEffectService.mAudioEffectState;
        initView();
        reportData(roomId);
    }

    protected void initView() {
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

    private void bindViewId() {
        mTextMusicVolume = findViewById(R.id.tv_music_volume);
        mTextEarReturnVolume = findViewById(R.id.tv_ear_return_volume);
        mTextPeopleVolume = findViewById(R.id.tv_people_volume);
    }

    private void initEarReturnVolume() {
        SeekBar seekEarReturnVolume = findViewById(R.id.sb_ear_return_volume);
        seekEarReturnVolume.setProgress(mAudioEffectState.earMonitorVolume.get());

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
        switchEarReturn.setChecked(mAudioEffectState.enableVoiceEarMonitor.get());
        switchEarReturn.setOnCheckedChangeListener(
                (button, enable) -> mAudioEffectService.enableVoiceEarMonitor(enable));
    }

    private void initPeopleVolumeView() {
        mTextPeopleVolume.setText(String.valueOf(mAudioEffectState.voiceVolume.get()));

        SeekBar seekPeopleVolume = findViewById(R.id.sb_people_volume);
        seekPeopleVolume.setProgress(mAudioEffectState.voiceVolume.get());
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
        mTextMusicVolume.setText((String.valueOf(mAudioEffectState.musicVolume.get())));

        SeekBar seekMusicVolume = findViewById(R.id.sb_music_volume);
        seekMusicVolume.setProgress(mAudioEffectState.musicVolume.get());
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
        mTextEarReturnVolume.setText((String.valueOf(mAudioEffectState.earMonitorVolume.get())));
    }

    private void initBackView() {
        findViewById(R.id.iv_back).setOnClickListener((view) -> {
            if (mOnBackButtonClickListener != null) {
                mOnBackButtonClickListener.onClick();
            }
        });
    }

    private void initReverbView() {
        RecyclerView recyclerReverb = findViewById(R.id.rv_reverb);
        ReverbAdapter adapterReverb = new ReverbAdapter(mContext, mAudioEffectService);
        int spanCount = adapterReverb.getItemCount();
        recyclerReverb.setLayoutManager(new GridLayoutManager(mContext, spanCount));
        recyclerReverb.setAdapter(adapterReverb);
    }

    private void initChangeVoiceView() {
        RecyclerView recyclerChangeVoice = findViewById(R.id.rv_change_voice);
        ChangeVoiceAdapter adapterChangeVoice = new ChangeVoiceAdapter(mContext, mAudioEffectService);
        int spanCount = adapterChangeVoice.getItemCount();
        recyclerChangeVoice.setLayoutManager(new GridLayoutManager(mContext, spanCount));
        recyclerChangeVoice.setAdapter(adapterChangeVoice);
    }

    public void setOnBackButtonClickListener(OnBackButtonClickListener listener) {
        mOnBackButtonClickListener = listener;
    }

    public interface OnBackButtonClickListener {
        void onClick();
    }

    private void reportData(String roomId) {
        boolean isVoiceRoom = !TextUtils.isEmpty(roomId) && roomId.startsWith("voice_");
        if (isVoiceRoom) {
            DataReporter.reportEventData(LIVEKIT_METRICS_PANEL_SHOW_VOICE_ROOM_AUDIO_EFFECT);
        } else {
            DataReporter.reportEventData(LIVEKIT_METRICS_PANEL_SHOW_LIVE_ROOM_AUDIO_EFFECT);
        }
    }
}
