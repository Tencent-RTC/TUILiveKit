//
//  MusicPanelState.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/4/30.
//

import Foundation

struct MusicInfo: Equatable {
    var id: Int32
    var name: String
    var path: String
    var isPlaying: Bool = false
    
    init() {
        self.id = -1
        self.name = ""
        self.path = ""
    }
    
    init(id: Int32, name: String, path: String) {
        self.id = id
        self.name = name
        self.path = path
    }
    
    static func == (lhs: MusicInfo, rhs: MusicInfo) -> Bool {
        return lhs.id == rhs.id && lhs.name == rhs.name
    }
    
    static func defaultMusicInfoList() -> [MusicInfo] {
        return [MusicInfo(id: 1,
                          name: .localized("live.music.cheerful"),
                          path: "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/PositiveHappyAdvertising.mp3"),
                MusicInfo(id: 2,
                          name: .localized("live.music.melancholy"),
                          path: "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/SadCinematicPiano.mp3"),
                MusicInfo(id: 3,
                          name: .localized("live.music.wonder.world"),
                          path: "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/WonderWorld.mp3"),]
    }
}

struct MusicPanelState {
    var currentPlayMusic: MusicInfo? = nil
    var musicInfoList: [MusicInfo] = [MusicInfo(id: 1,
                                                name: .localized("live.music.cheerful"),
                                                path: "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/PositiveHappyAdvertising.mp3"),
                                      MusicInfo(id: 2,
                                                name: .localized("live.music.melancholy"),
                                                path: "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/SadCinematicPiano.mp3"),
                                      MusicInfo(id: 3,
                                                name: .localized("live.music.wonder.world"),
                                                path: "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/WonderWorld.mp3"),]
}
