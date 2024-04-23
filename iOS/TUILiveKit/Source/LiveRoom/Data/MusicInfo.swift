//
//  File.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/20.
//

import Foundation

class MusicInfo {
    var id: Int32
    var name: String
    var path: String
    let isPlaying: Observable<Bool> = Observable(false)
    var pitch: Float = 0.0

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

    static func defaultMusicInfoList() -> [MusicInfo] {
        return [MusicInfo(id: 1,
                          name: .localized("live.music.cheerful"),
                          path: "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/PositiveHappyAdvertising.mp3"),
                MusicInfo(id: 2,
                          name: .localized("live.music.melancholy"),
                          path: "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/SadCinematicPiano.mp3"),
                MusicInfo(id: 3,
                          name: .localized("live.music.wonder.world"),
                          path: "https://dldir1.qq.com/hudongzhibo/TUIKit/resource/music/WonderWorld.mp3"), ]
    }
}
