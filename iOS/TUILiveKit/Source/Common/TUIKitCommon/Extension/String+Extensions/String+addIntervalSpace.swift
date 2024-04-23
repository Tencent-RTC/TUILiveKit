//
//  String+addIntervalSpace.swift
//  RTCubeApp
//
//  Created by krabyuon 2023/8/9.
//  Copyright © 2023 Tencent. All rights reserved.
//

import Foundation

extension String {
    public func addIntervalSpace(intervalStr: String, interval: Int) -> String {
        var output = ""
        enumerated().forEach { index, c in
            if (index % interval == 0) && index > 0 {
                output += intervalStr
            }
            output.append(c)
        }
        return output
    }
}
