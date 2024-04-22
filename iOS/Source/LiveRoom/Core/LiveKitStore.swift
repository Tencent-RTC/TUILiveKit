//
//  LiveKitStore.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/11.
//  Copyright © 2023 Tencent. All rights reserved.
//

import Foundation
import TUICore
import Combine

class LiveKitStore {
    @Published var selfInfo: UserInfo = UserInfo()
    @Published var applyLinkAudienceList: [UserInfo] = []
    let maxSeatCount: Int = 9
    let musicList: Observable<[MusicInfo]> = Observable(MusicInfo.defaultMusicInfoList())
    let currentMusicInfo: Observable<MusicInfo> = Observable(MusicInfo())
    let isPortrait: Observable<Bool> = Observable(true)
}
