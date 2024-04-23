//
//  AudioInfo.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/11.
//  Copyright © 2023 Tencent. All rights reserved.
//

import Foundation
import RTCRoomEngine
#if TXLiteAVSDK_TRTC
    import TXLiteAVSDK_TRTC
#elseif TXLiteAVSDK_Professional
    import TXLiteAVSDK_Professional
#endif

class AudioInfo {
    var musicVolume: Observable<Int> = Observable(60)
    var voiceVolume: Observable<Int> = Observable(60)
    var muteAudio: Observable<Bool> = Observable(false)
    var enableVoiceEarMonitor: Observable<Bool> = Observable(false)
    var changerType: Observable<TXVoiceChangeType> = Observable(._0)
    var reverbType: Observable<TXVoiceReverbType> = Observable(._0)
    var audioQuality: Observable<TUIAudioQuality> = Observable(.default)
}
