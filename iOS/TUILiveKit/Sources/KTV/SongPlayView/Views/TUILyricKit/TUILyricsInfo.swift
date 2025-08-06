//
//  TUILyricsInfo.swift
//
//  Created by adams on 2021/7/16.
//  Copyright © 2022 Tencent. All rights reserved.

import Foundation

public struct TUILyricsInfo: Equatable {
    var lyricLineInfos: [TUILyricsLineInfo]
}

public struct TUILyricsLineInfo: Equatable {
    var charStrArray: [TUILyricsCharacterInfo]
    var startTime: Double
    var endTime: Double
}

public struct TUILyricsCharacterInfo: Equatable {
    let startTime: Int
    let duration: Int
    var endTime: Int {
        return startTime + duration
    }
    let characterStr: String
}

public class TUILyricParser: NSObject {
    static var isVtt: Bool = false
    static var lyricsLineInfo: TUILyricsLineInfo = TUILyricsLineInfo(charStrArray: [],
                                                                     startTime: 0,
                                                                     endTime: 0)
    
    static func parserLocalLyricFile(fileURL: URL) -> TUILyricsInfo? {
        guard FileManager.default.fileExists(atPath: fileURL.path) else {
            return nil
        }
        guard let lyricsStr = try? String(contentsOfFile: fileURL.path) else {
            return nil
        }
        let lyricsArray = lyricsStr.components(separatedBy: "\n")
        var lyricsInfo = TUILyricsInfo(lyricLineInfos: [])
        for str in lyricsArray {
            if str.contains("-->") {
                let timeArray = str.replacingOccurrences(of: " ", with: "").components(separatedBy: "-->")
                if timeArray.count == 2 {
                    let startTimeArray = timeArray[0].components(separatedBy: ":")
                    let endTimeArray = timeArray[1].components(separatedBy: ":")
                    guard let smin = Double(startTimeArray[1]), let ssec = Double(startTimeArray[2]) else { continue }
                    guard let emin = Double(endTimeArray[1]), let esec = Double(endTimeArray[2]) else { continue }
                    let startTime = smin * 60 + ssec
                    let endTime = emin * 60 + esec
                    lyricsLineInfo.startTime = startTime
                    lyricsLineInfo.endTime = endTime
                    isVtt = true
                }
            }
            
            if str.hasPrefix("<") {
                let lyricsLineArray = str.components(separatedBy: "<")
                for lineStr in lyricsLineArray {
                    if lineStr.count > 0 {
                        let line = lineStr.replacingOccurrences(of: ",0>", with: ",")
                        let characterArray = line.components(separatedBy: ",")
                        guard characterArray.count == 3 else { continue }
                        guard let startTime = Int(characterArray[0]), var duration = Int(characterArray[1]) else {
                            continue
                        }
                        let characterStr = characterArray[2]
                        if isVtt {
                            if duration == 0 {
                                // 歌词解析时 可能出现duration为0的情况 此时需要给一个默认值：100(ms)
                                duration = 10
                            }
                            lyricsLineInfo.charStrArray.append(TUILyricsCharacterInfo(startTime: startTime,
                                                                                      duration: duration,
                                                                                      characterStr: characterStr))
                        }
                    }
                }
                isVtt = false
            }
            
            if !isVtt && lyricsLineInfo.charStrArray.count > 0 {
                let lineLyricModelTemp = TUILyricsLineInfo(charStrArray: lyricsLineInfo.charStrArray,
                                                           startTime: lyricsLineInfo.startTime,
                                                           endTime: lyricsLineInfo.endTime)
                lyricsInfo.lyricLineInfos.append(lineLyricModelTemp)
                lyricsLineInfo.charStrArray.removeAll()
                lyricsLineInfo.startTime = 0
            }
        }
        
        if lyricsInfo.lyricLineInfos.count == 0 {
            return nil
        }
        return lyricsInfo
    }
    
}
