//
//  KaraokeState.swift
//  Pods
//
//  Created by ssc on 2025/8/19.
//

import Foundation
#if canImport(TXLiteAVSDK_TRTC)
import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
import TXLiteAVSDK_Professional
#endif

enum PlaybackState {
    case idel
    case start
    case pause
    case resume
    case stop
}

class KaraokeState {
    var currentMusicId: String = ""
    var currentMusicTotalDuration: TimeInterval = 0
    var chorusRole: TXChorusRole = .leadSinger
    var songLibrary: [MusicInfo] = availableSongs1
    var selectedSongs: [SelectedMusicInfo] = []

    var musicTrackType: TXChorusMusicTrack = .originalSong
    var playProgress: TimeInterval = 0
    var playbackState: PlaybackState = .stop

    var standardPitchSequence: [TXReferencePitch] = []
    var pitch: Int32 = 0
    var currentScore: Int32 = 0
    var averageScore: Int32 = 0

    var EarMonitor: Bool = false
    var enableScore: Bool = false
    var publishVolume: Int = 60
    var playoutVolume: Int = 95
    var musicPitch: Float = 0.0
    var reverbType: MusicReverbType = .none
    var enableRequestMusic: Bool = true
    func isSongSelected(_ musicId: String) -> Bool {
        return selectedSongs.contains { $0.musicId == musicId }
    }
}

struct MusicInfo: Equatable{
    let musicId: String
    let musicName: String
    let artist: String
    let duration: TimeInterval
    let coverUrl: String
    let accompanyUrl: String
    let originalUrl: String
    let lyricUrl: String
    let isOriginal: Bool
    let hasRating: Bool
}

struct SelectedMusicInfo: Equatable, Codable{
    let musicId: String
    let userId: String
    let userName: String
    let avatarUrl: String
}

enum MusicReverbType: Int {
    case none = 0
    case KTV = 1
    case smallRoom = 2
    case auditorium = 3
    case deep = 4
    case loud = 5
    case metallic = 6
    case magnetic = 7
}


let availableSongs1: [MusicInfo] = []
