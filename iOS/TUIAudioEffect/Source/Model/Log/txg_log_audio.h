//
//  txg_log_audio.h
//  Pods
//
//  Created by 林智 on 2020/11/13.
//

#ifndef txg_log_audio_h
#define txg_log_audio_h

#define TUIAudioEffectLog(fmt, ...) NSLog((@"TUIAudioEffect LOG:%s [Line %d] " fmt), __PRETTY_FUNCTION__, __LINE__, ##__VA_ARGS__)

#define LOGE(fmt, ...) \
    TUIAudioEffectLog(fmt, ##__VA_ARGS__)
#define LOGW(fmt, ...) \
    TUIAudioEffectLog(fmt, ##__VA_ARGS__)
#define LOGI(fmt, ...) \
    TUIAudioEffectLog(fmt, ##__VA_ARGS__)
#define LOGD(fmt, ...) \
    TUIAudioEffectLog(fmt, ##__VA_ARGS__)
#define LOGV(fmt, ...) \
    TUIAudioEffectLog(fmt, ##__VA_ARGS__)

#endif /* txg_log_audio_h */
