//
//  MusicPitchModel.swift
//  TUIKaraoke
//
//  Created by adams on 2023/6/5.
//

import Foundation

public class MusicPitchModel: NSObject {
    var startTime: Int
    var duration: Int
    var pitch: Int
    
    public init(startTime: Int, duration: Int, pitch: Int) {
        self.startTime = startTime
        self.duration = duration
        self.pitch = pitch
    }
}
