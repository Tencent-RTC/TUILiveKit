//
//  MusicSettingManager.swift
//  TUILiveKit
//
//  Created by gg on 2024/12/9.
//

import Foundation
import RTCCommon
import Combine
import RTCRoomEngine

class MusicSettingManager {
    private weak var karaokeManager: KaraokeManager?
    private lazy var provider = MusicSettingDataProvider(
        manager: self.karaokeManager ?? KaraokeManager(roomId: "")
    )

    init(karaokeManager: KaraokeManager) {
        self.karaokeManager = karaokeManager
    }
}

// MARK: - MusicSettingMenuDateGenerator
extension MusicSettingManager: MusicSettingMenuDateGenerator {
    var MusicSettingMenus: [Section : [MusicSettingItem]] {
        provider.MusicSettingMenus
    }
    
    var MusicSettingSectionTitles: [Section : String] {
        provider.MusicSettingSectionTitles
    }
}


