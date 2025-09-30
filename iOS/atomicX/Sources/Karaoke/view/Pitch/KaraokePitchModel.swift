//
//  KaraokePitchModel.swift
//  Pods
//
//  Created by ssc on 2025/8/22.
//

import Foundation

public class KaraokePitchModel: NSObject {
    var startTime: Int
    var duration: Int
    var pitch: Int

    public init(startTime: Int, duration: Int, pitch: Int) {
        self.startTime = startTime
        self.duration = duration
        self.pitch = pitch
    }

    public static func models(from jsonData: [String: Any]) -> [KaraokePitchModel] {
        var models = [KaraokePitchModel]()

        let sortedFrames = jsonData.keys.sorted { key1, key2 in
            let index1 = Int(key1.replacingOccurrences(of: "frame_", with: "")) ?? 0
            let index2 = Int(key2.replacingOccurrences(of: "frame_", with: "")) ?? 0
            return index1 < index2
        }

        for frameKey in sortedFrames {
            guard let frameData = jsonData[frameKey] as? [String: Any],
                  let start = frameData["start_time"] as? Double,
                  let end = frameData["end_time"] as? Double,
                  let midiPitch = frameData["pitch_midi"] as? Int else {
                continue
            }

            let startTime = Int(start * 1000)
            let duration = Int((end - start) * 1000)

            if midiPitch > 0 && duration > 0 {
                models.append(KaraokePitchModel(
                    startTime: startTime,
                    duration: duration,
                    pitch: midiPitch
                ))
            }
        }

        return models
    }
}
