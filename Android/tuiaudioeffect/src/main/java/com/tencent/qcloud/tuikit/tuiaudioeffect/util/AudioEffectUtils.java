package com.tencent.qcloud.tuikit.tuiaudioeffect.util;

import com.tencent.liteav.audio.TXAudioEffectManager;

public class AudioEffectUtils {

    public static final String EXTENSION_AUDIOEFFECT = "extension_audioeffect";

    public static final String KEY_CONTEXT              = "context";
    public static final String KEY_AUDIOEFFECTMANAGER   = "audioeffectmanager";
    public static final String KEY_AUDIOEFFECTEXTENSION = "audioEffectExtension";

    /*******************  变声类型  *****************/
    public static final int AUDIO_VOICECHANGER_TYPE_0  = 0;
    public static final int AUDIO_VOICECHANGER_TYPE_1  = 1;
    public static final int AUDIO_VOICECHANGER_TYPE_2  = 2;
    public static final int AUDIO_VOICECHANGER_TYPE_3  = 3;
    public static final int AUDIO_VOICECHANGER_TYPE_4  = 4;
    public static final int AUDIO_VOICECHANGER_TYPE_5  = 5;
    public static final int AUDIO_VOICECHANGER_TYPE_6  = 6;
    public static final int AUDIO_VOICECHANGER_TYPE_7  = 7;
    public static final int AUDIO_VOICECHANGER_TYPE_8  = 8;
    public static final int AUDIO_VOICECHANGER_TYPE_9  = 9;
    public static final int AUDIO_VOICECHANGER_TYPE_10 = 10;
    public static final int AUDIO_VOICECHANGER_TYPE_11 = 11;

    /*******************  混响类型  *****************/
    public static final int AUDIO_REVERB_TYPE_0 = 0;
    public static final int AUDIO_REVERB_TYPE_1 = 1;
    public static final int AUDIO_REVERB_TYPE_2 = 2;
    public static final int AUDIO_REVERB_TYPE_3 = 3;
    public static final int AUDIO_REVERB_TYPE_4 = 4;
    public static final int AUDIO_REVERB_TYPE_5 = 5;
    public static final int AUDIO_REVERB_TYPE_6 = 6;
    public static final int AUDIO_REVERB_TYPE_7 = 7;

    /*******************  歌曲地址  *****************/
    public static final String ONLINE_BGM_FIRST  = "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/PositiveHappyAdvertising.mp3";
    public static final String ONLINE_BGM_SECOND = "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/SadCinematicPiano.mp3";
    public static final String ONLINE_BGM_THIRD  = "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/WonderWorld.mp3";


    public static TXAudioEffectManager.TXVoiceReverbType translateReverbType(int type) {
        TXAudioEffectManager.TXVoiceReverbType reverbType =
                TXAudioEffectManager.TXVoiceReverbType.TXLiveVoiceReverbType_0;
        switch (type) {
            case AUDIO_REVERB_TYPE_0:
                reverbType = TXAudioEffectManager.TXVoiceReverbType.TXLiveVoiceReverbType_0;
                break;
            case AUDIO_REVERB_TYPE_1:
                reverbType = TXAudioEffectManager.TXVoiceReverbType.TXLiveVoiceReverbType_1;
                break;
            case AUDIO_REVERB_TYPE_2:
                reverbType = TXAudioEffectManager.TXVoiceReverbType.TXLiveVoiceReverbType_2;
                break;
            case AUDIO_REVERB_TYPE_3:
                reverbType = TXAudioEffectManager.TXVoiceReverbType.TXLiveVoiceReverbType_3;
                break;
            case AUDIO_REVERB_TYPE_4:
                reverbType = TXAudioEffectManager.TXVoiceReverbType.TXLiveVoiceReverbType_4;
                break;
            case AUDIO_REVERB_TYPE_5:
                reverbType = TXAudioEffectManager.TXVoiceReverbType.TXLiveVoiceReverbType_5;
                break;
            case AUDIO_REVERB_TYPE_6:
                reverbType = TXAudioEffectManager.TXVoiceReverbType.TXLiveVoiceReverbType_6;
                break;
            case AUDIO_REVERB_TYPE_7:
                reverbType = TXAudioEffectManager.TXVoiceReverbType.TXLiveVoiceReverbType_7;
                break;
            default:
                break;
        }
        return reverbType;
    }

    public static TXAudioEffectManager.TXVoiceChangerType translateChangerType(int type) {
        TXAudioEffectManager.TXVoiceChangerType changerType =
                TXAudioEffectManager.TXVoiceChangerType.TXLiveVoiceChangerType_0;
        switch (type) {
            case AUDIO_VOICECHANGER_TYPE_0:
                changerType = TXAudioEffectManager.TXVoiceChangerType.TXLiveVoiceChangerType_0;
                break;
            case AUDIO_VOICECHANGER_TYPE_1:
                changerType = TXAudioEffectManager.TXVoiceChangerType.TXLiveVoiceChangerType_1;
                break;
            case AUDIO_VOICECHANGER_TYPE_2:
                changerType = TXAudioEffectManager.TXVoiceChangerType.TXLiveVoiceChangerType_2;
                break;
            case AUDIO_VOICECHANGER_TYPE_3:
                changerType = TXAudioEffectManager.TXVoiceChangerType.TXLiveVoiceChangerType_3;
                break;
            case AUDIO_VOICECHANGER_TYPE_4:
                changerType = TXAudioEffectManager.TXVoiceChangerType.TXLiveVoiceChangerType_4;
                break;
            case AUDIO_VOICECHANGER_TYPE_5:
                changerType = TXAudioEffectManager.TXVoiceChangerType.TXLiveVoiceChangerType_5;
                break;
            case AUDIO_VOICECHANGER_TYPE_6:
                changerType = TXAudioEffectManager.TXVoiceChangerType.TXLiveVoiceChangerType_6;
                break;
            case AUDIO_VOICECHANGER_TYPE_7:
                changerType = TXAudioEffectManager.TXVoiceChangerType.TXLiveVoiceChangerType_7;
                break;
            case AUDIO_VOICECHANGER_TYPE_8:
                changerType = TXAudioEffectManager.TXVoiceChangerType.TXLiveVoiceChangerType_8;
                break;
            case AUDIO_VOICECHANGER_TYPE_9:
                changerType = TXAudioEffectManager.TXVoiceChangerType.TXLiveVoiceChangerType_9;
                break;
            case AUDIO_VOICECHANGER_TYPE_10:
                changerType = TXAudioEffectManager.TXVoiceChangerType.TXLiveVoiceChangerType_10;
                break;
            case AUDIO_VOICECHANGER_TYPE_11:
                changerType = TXAudioEffectManager.TXVoiceChangerType.TXLiveVoiceChangerType_11;
                break;
            default:
                break;
        }
        return changerType;
    }

    public static String formattedTime(long second) {
        String hs;
        long h = second / 3600;
        if (h < 10) {
            hs = "0" + h;
        } else {
            hs = "" + h;
        }

        String ms;
        long m = (second % 3600) / 60;
        if (m < 10) {
            ms = "0" + m;
        } else {
            ms = "" + m;
        }

        String ss;
        long s = (second % 3600) % 60;
        if (s < 10) {
            ss = "0" + s;
        } else {
            ss = "" + s;
        }

        String formatTime;
        if (h > 0) {
            formatTime = hs + ":" + ms + ":" + ss;
        } else {
            formatTime = ms + ":" + ss;
        }
        return formatTime;
    }
}
