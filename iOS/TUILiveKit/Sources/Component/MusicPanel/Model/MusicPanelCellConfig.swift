//
//  MusicPanelCellConfig.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/4/28.
//

import Foundation
import Combine

struct MusicInfoCellItem {
    var title: String
    var musicInfo: MusicInfo
    
    var startPlay: ((MusicInfo) -> Void)?
    var stopPlay: ((MusicInfo) -> Void)?
    var deleteMusic:((MusicInfo) -> Void)?
}
