//
//  MusicServiceObserver.swift
//  TUIKaraoke
//
//  Created by adams on 2023/4/3.
//

import UIKit

public protocol MusicServiceObserver: AnyObject {

    func onMusicListChanged(musicInfoList: [QueuedSong])
    
    func onMusicScorePrepared(pitchModelList: [MusicPitchModel])
    
    func onMusicScoreFinished(totalScore: Int32)
    
    func onMusicRealTimeProgress(progress: Int)
    
    func onMusicRealTimePitch(pitch: Int)
    
    func onMusicSingleScore(currentScore: Int32)
}
