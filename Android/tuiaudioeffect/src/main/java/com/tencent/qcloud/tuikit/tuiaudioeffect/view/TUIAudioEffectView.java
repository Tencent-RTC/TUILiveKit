package com.tencent.qcloud.tuikit.tuiaudioeffect.view;

import static android.view.View.GONE;
import static android.view.View.VISIBLE;

import android.app.Dialog;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.util.TypedValue;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CompoundButton;
import android.widget.ImageButton;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.SeekBar;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.SwitchCompat;

import com.tencent.qcloud.tuikit.tuiaudioeffect.R;
import com.tencent.qcloud.tuikit.tuiaudioeffect.model.BGMItemEntity;
import com.tencent.qcloud.tuikit.tuiaudioeffect.model.VoiceItemEntity;
import com.tencent.qcloud.tuikit.tuiaudioeffect.presenter.IAudioEffectPresenter;
import com.tencent.qcloud.tuikit.tuiaudioeffect.util.AudioEffectUtils;
import com.tencent.qcloud.tuikit.tuiaudioeffect.view.internal.BaseSeekView;
import com.tencent.qcloud.tuikit.tuiaudioeffect.view.internal.MusicSelectView;
import com.tencent.qcloud.tuikit.tuiaudioeffect.view.internal.MusicVolumeView;
import com.tencent.qcloud.tuikit.tuiaudioeffect.view.internal.VoicePitchView;
import com.tencent.qcloud.tuikit.tuiaudioeffect.view.internal.VoiceRecyclerView;
import com.tencent.qcloud.tuikit.tuiaudioeffect.view.internal.VoiceVolumeView;

import java.util.List;
import java.util.Locale;

/**
 * 音效面板 Dialog
 */
public class TUIAudioEffectView extends Dialog {

    private static final String TAG = "TUIAudioEffectView";

    private Context      mContext;
    private Button       mButtonSelectedSong;
    private ImageView    mImageSelectedSong;
    private TextView     mTextBGMBack;
    private LinearLayout mPanelMainAudioEffect;
    private LinearLayout mPanelBGM;
    private TextView     mTextStartTime;
    private TextView     mTextTotalTime;
    private TextView     mTextBGM;
    private TextView     mTextActor;
    private TextView     mTextMusicDescription;
    private SwitchCompat mSwitchMusiceAudiction;
    private LinearLayout mLayoutSelectBGM;
    private LinearLayout mMainPanel;
    private ImageButton  mButtonBGMPlay;

    private MusicVolumeView mViewMusicVolume; // 音乐音量滑动条
    private VoiceVolumeView mViewVoiceVolume; // 人声音量滑动条
    private VoicePitchView  mViewVoicePitch;  // 声音升降调滑动条

    private VoiceRecyclerView mViewVoiceChange; // 变声选择器
    private VoiceRecyclerView mViewVoiceReverb; // 混响选择器
    private MusicSelectView   mViewMusicSelect; // 歌曲选择器

    private int     mBGMId     = -1;
    private boolean mIsPlaying = false;
    private boolean mIsPause   = false;
    private boolean mIsPlayEnd = false;

    private boolean mRefreshedScrollView = false; // 是否刷新过ScrollView

    private IAudioEffectPresenter mPresenter;

    private final Handler mHandler = new Handler(Looper.getMainLooper());


    public TUIAudioEffectView(@NonNull Context context, IAudioEffectPresenter presenter) {
        super(context, R.style.TUIAudioEffectDialogTheme);
        setContentView(R.layout.tuiaudioeffect_panel);
        mContext = context;
        mPresenter = presenter;
        initView();
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        getWindow().setLayout(ViewGroup.LayoutParams.MATCH_PARENT, getMaxHeight());
        getWindow().setGravity(Gravity.BOTTOM);
    }

    @Override
    public void onWindowFocusChanged(boolean hasFocus) {
        super.onWindowFocusChanged(hasFocus);
        if (!mRefreshedScrollView) {
            mRefreshedScrollView = true;
            // 小米11手机上，需要主动设置初始位置。
            final ScrollView scrollView = findViewById(R.id.sv_content);
            scrollView.scrollTo(0, 0);
        }
    }

    private int getMaxHeight() {
        return (int) (getContext().getResources().getDisplayMetrics().heightPixels * 0.4);
    }

    public void hideManagerView() {
        mLayoutSelectBGM.setVisibility(GONE);
        mViewVoiceChange.setVisibility(GONE);
        mViewVoicePitch.setVisibility(GONE);
    }

    private void initView() {
        mViewMusicVolume = findViewById(R.id.sb_bgm_volume);
        mViewVoiceVolume = findViewById(R.id.sb_mic_volume);
        mViewVoicePitch = findViewById(R.id.sb_pitch_level);
        mViewVoiceChange = findViewById(R.id.audio_change_type_rv);
        mViewVoiceReverb = findViewById(R.id.audio_reverb_type_rv);
        mViewMusicSelect = findViewById(R.id.audio_bgm_rv);
        mMainPanel = findViewById(R.id.ll_panel);
        mSwitchMusiceAudiction = findViewById(R.id.switch_music_audition);
        mTextActor = findViewById(R.id.tv_actor);
        mTextStartTime = findViewById(R.id.tv_bgm_start_time);
        mTextTotalTime = findViewById(R.id.tv_bgm_end_time);
        mTextMusicDescription = findViewById(R.id.music_description);
        mButtonBGMPlay = findViewById(R.id.ib_audio_bgm_play);
        mTextBGM = findViewById(R.id.tv_bgm);
        mLayoutSelectBGM = findViewById(R.id.ll_select_bgm);
        mButtonSelectedSong = findViewById(R.id.audio_btn_select_song);
        mImageSelectedSong = findViewById(R.id.iv_select_song);
        mTextBGMBack = findViewById(R.id.tv_back);
        mPanelMainAudioEffect = findViewById(R.id.audio_main_ll);
        mPanelBGM = findViewById(R.id.audio_main_bgm);

        if (!isZh(mContext)) {
            mTextMusicDescription.setTextSize(TypedValue.COMPLEX_UNIT_SP, 11);
        }

        initListener();
        initData();
    }

    private void initListener() {
        findViewById(R.id.link_music).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                Intent intent = new Intent(Intent.ACTION_VIEW);
                intent.setData(Uri.parse("https://cloud.tencent.com/product/ame"));
                mContext.startActivity(intent);
            }
        });

        mSwitchMusiceAudiction.setChecked(mPresenter.isEnableVoiceEarMonitor());
        mSwitchMusiceAudiction.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
            @Override
            public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
                mPresenter.enableVoiceEarMonitor(isChecked);
            }
        });

        mTextBGMBack.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                onSelectSong(false);
            }
        });

        View.OnClickListener selectSongListener = new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                onSelectSong(true);
            }
        };
        mLayoutSelectBGM.setOnClickListener(selectSongListener);
        mButtonSelectedSong.setOnClickListener(selectSongListener);

        mViewMusicVolume.setOnSeekBarChangeListener(new BaseSeekView.AbsOnSeekBarChangeListener() {
            @Override
            public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
                if (mPresenter != null && mBGMId != -1) {
                    mPresenter.setMusicPlayoutVolume(mBGMId, mViewMusicVolume.getVolume());
                    mPresenter.setMusicPublishVolume(mBGMId, mViewMusicVolume.getVolume());
                }
            }
        });

        mViewVoiceVolume.setOnSeekBarChangeListener(new BaseSeekView.AbsOnSeekBarChangeListener() {
            @Override
            public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
                if (mPresenter != null) {
                    mPresenter.setVoiceCaptureVolume(mViewVoiceVolume.getVolume());
                }
            }
        });

        mViewVoicePitch.setOnSeekBarChangeListener(new BaseSeekView.AbsOnSeekBarChangeListener() {
            @Override
            public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
                if (mPresenter != null && mBGMId != -1) {
                    mPresenter.setMusicPitch(mBGMId, mViewVoicePitch.getPitch());
                }
            }
        });
    }

    private void initData() {
        final List<VoiceItemEntity> changerItemEntityList = mPresenter.getVoiceChangeData();
        VoiceRecyclerView.RecyclerViewAdapter changerAdapter = new VoiceRecyclerView.RecyclerViewAdapter(
                changerItemEntityList, new VoiceRecyclerView.OnItemClickListener() {
            @Override
            public void onItemClick(int pos) {
                int type = ((VoiceRecyclerView.RecyclerViewAdapter) mViewVoiceChange.getAdapter()).getItem(pos).mType;
                Log.d(TAG, "select changer type " + type);
                if (mPresenter != null) {
                    mPresenter.setVoiceChangerType(type);
                }
            }
        });
        changerAdapter.setSelectPosition(0);
        mViewVoiceChange.setAdapter(changerAdapter);

        final List<VoiceItemEntity> reverbItemEntityList = mPresenter.getVoiceReverbData();
        VoiceRecyclerView.RecyclerViewAdapter reverbAdapter = new VoiceRecyclerView.RecyclerViewAdapter(
                reverbItemEntityList, new VoiceRecyclerView.OnItemClickListener() {
            @Override
            public void onItemClick(int pos) {
                int type = ((VoiceRecyclerView.RecyclerViewAdapter) mViewVoiceReverb.getAdapter()).getItem(pos).mType;
                Log.d(TAG, "select reverb type " + type);
                if (mPresenter != null) {
                    mPresenter.setVoiceReverbType(type);
                }
            }
        });
        reverbAdapter.setSelectPosition(0);
        mViewVoiceReverb.setAdapter(reverbAdapter);

        final List<BGMItemEntity> songItemEntityList = mPresenter.getSongData();
        MusicSelectView.RecyclerViewAdapter songAdapter = new MusicSelectView.RecyclerViewAdapter(
                songItemEntityList, new MusicSelectView.OnItemClickListener() {
            @Override
            public void onItemClick(int pos) {
                handleBGM(pos, ((MusicSelectView.RecyclerViewAdapter) mViewMusicSelect.getAdapter()).getItem(pos));
            }
        });
        mViewMusicSelect.setAdapter(songAdapter);
    }

    private void onSelectSong(boolean select) {
        if (select) {
            mPanelMainAudioEffect.setVisibility(GONE);
            mPanelBGM.setVisibility(VISIBLE);
        } else {
            mPanelMainAudioEffect.setVisibility(VISIBLE);
            mPanelBGM.setVisibility(GONE);
        }
        int height = select ? ViewGroup.LayoutParams.WRAP_CONTENT : getMaxHeight();
        getWindow().setLayout(ViewGroup.LayoutParams.MATCH_PARENT, height);
    }

    @Override
    public void show() {
        super.show();
        mSwitchMusiceAudiction.setChecked(mPresenter.isEnableVoiceEarMonitor());
    }

    private void handleBGM(final int position, final BGMItemEntity model) {
        if (mPresenter == null) {
            return;
        }
        if (mBGMId != -1) { // 已开始播放音乐，需要先停止上一次正在播放的音乐
            mPresenter.stopPlayMusic(mBGMId);
        }
        mBGMId = position;
        onSelectSong(false);
        mTextBGM.setVisibility(GONE);
        mButtonSelectedSong.setVisibility(GONE);
        mImageSelectedSong.setVisibility(GONE);
        mTextActor.setVisibility(VISIBLE);
        mTextActor.setText(model.mTitle);
        mTextStartTime.setVisibility(VISIBLE);
        mTextTotalTime.setVisibility(VISIBLE);
        mButtonBGMPlay.setVisibility(VISIBLE);
        mButtonBGMPlay.setImageResource(R.drawable.tuiaudioeffect_bgm_pause);
        // 开始播放音乐时，无论是否首次均需重新设置变调和音量，因为音乐id发生了变化
        mPresenter.setMusicPitch(position, mViewVoicePitch.getPitch());
        mPresenter.setMusicPlayoutVolume(position, mViewMusicVolume.getVolume());
        mPresenter.setMusicPublishVolume(position, mViewMusicVolume.getVolume());
        mPresenter.setMusicObserver(mBGMId, new BGMListener());
        mHandler.post(new Runnable() {
            @Override
            public void run() {
                // 此处调用sdk接口，所以略微耗时。post一下再执行可以避免UI卡顿。
                String hint = AudioEffectUtils.formattedTime(mPresenter.getMusicDurationInMS(model.mPath) / 1000);
                mTextTotalTime.setText("/" + hint + "");
                mPresenter.startPlayMusic(position, model.mPath, true);
            }
        });
        mIsPlaying = true;
        mIsPause = false;

        mButtonBGMPlay.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (mIsPlayEnd) {
                    mPresenter.startPlayMusic(position, model.mPath, true);
                    mButtonBGMPlay.setImageResource(R.drawable.tuiaudioeffect_bgm_pause);
                    mIsPlayEnd = false;
                    mIsPlaying = true;
                    mIsPause = false;
                } else if (mIsPlaying) {
                    mPresenter.pausePlayMusic(mBGMId);
                    mButtonBGMPlay.setImageResource(R.drawable.tuiaudioeffect_bgm_play);
                    mIsPlaying = false;
                    mIsPause = true;
                } else {
                    mPresenter.resumePlayMusic(mBGMId);
                    mButtonBGMPlay.setImageResource(R.drawable.tuiaudioeffect_bgm_pause);
                    mIsPlaying = true;
                    mIsPause = false;
                }
            }
        });
    }

    public boolean isZh(Context context) {
        Locale locale = context.getResources().getConfiguration().locale;
        String language = locale.getLanguage();
        return language.endsWith("zh");
    }

    public void setPanelBackgroundColor(int color) {
        mMainPanel.setBackgroundColor(color);
    }

    private class BGMListener implements IAudioEffectPresenter.IMusicPlayObserver {

        @Override
        public void onStart(int i, int i1) {
            mIsPlayEnd = false;
        }

        @Override
        public void onPlayProgress(int id, final long curPtsMS, long durationMS) {
            mHandler.post(new Runnable() {
                @Override
                public void run() {
                    mTextStartTime.setText(AudioEffectUtils.formattedTime(curPtsMS / 1000) + "");
                }
            });
        }

        @Override
        public void onComplete(int id, int i1) {
            Log.d(TAG, "onMusicPlayFinish id " + id);
            mHandler.post(new Runnable() {
                @Override
                public void run() {
                    // 播放完成更新状态
                    mButtonBGMPlay.setVisibility(VISIBLE);
                    mButtonBGMPlay.setImageResource(R.drawable.tuiaudioeffect_bgm_play);
                    mIsPlayEnd = true;
                }
            });
        }
    }
}
