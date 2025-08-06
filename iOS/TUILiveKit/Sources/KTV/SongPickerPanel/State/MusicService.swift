//
//  KaraokeMusicService.swift
//  TUIKaraoke
//
//  Created by gg on 2021/7/8.
//  Copyright © 2022 Tencent. All rights reserved.

import Foundation

public typealias KaraokeCallback = (_ code: Int, _ message: String) -> ()


public typealias MusicListCallback = (_ errorCode: NSInteger, _ errorMessage: String, _ list: [Song], _ scrollToken: String) -> Void

public typealias MusicTagListCallback = (_ errorCode: NSInteger, _ errorMessage: String, _ list: [MusicTagModel]) -> Void

public typealias MusicSelectedListCallback = (_ errorCode: NSInteger, _ errorMessage: String, _ list: [QueuedSong]) -> Void

public typealias MusicStartCallback = (_ musicInfo: QueuedSong) -> Void

public typealias MusicProgressCallback = (_ musicInfo: QueuedSong?, _ progress: Float) -> Void

public typealias MusicFinishCallback = (_ musicInfo: QueuedSong, _ errorCode: Int32, _ msg: String) -> Void

public typealias KaraokeAddMusicCallback = (start: MusicStartCallback, progress: MusicProgressCallback, finish: MusicFinishCallback)

public protocol KaraokeMusicService: AnyObject {
    
    func addObserver(_ observer: MusicServiceObserver)
    
    func destroyService()
    
    func setRoomId(_ roomId: String)
    
    func getMusicTagList(callback: @escaping MusicTagListCallback)
    
    func getMusicsByTagId(tagId: String, scrollToken: String, callback: @escaping MusicListCallback)
    
    func getMusicsByKeywords(keyWord: String, limit: Int, scrollToken: String, callback: @escaping MusicListCallback)
    
    func getPlaylist(_ callback: @escaping MusicSelectedListCallback)
    
    func addMusicToPlaylist(musicInfo: QueuedSong, callBack: KaraokeAddMusicCallback?)
    
    func deleteMusicFromPlaylist(musicInfo: QueuedSong, callback: @escaping KaraokeCallback)
    
    func clearPlaylistByUserId(userID: String, callback: @escaping KaraokeCallback)
    
    func topMusic(musicInfo: QueuedSong, callback: @escaping KaraokeCallback)
    
    func switchMusicFromPlaylist(musicInfo: QueuedSong, callback: @escaping KaraokeCallback)
    
    func completePlaying(musicInfo: QueuedSong, callback: @escaping KaraokeCallback)
    
    
    // MARK: - score
    func prepareMusicScore(musicInfo: QueuedSong)
    
    func processMusicScore(buffer: UnsafeMutablePointer<CChar>, length: Int32, timeStamp: Double)
 
    func finishMusicScore()
    
}
