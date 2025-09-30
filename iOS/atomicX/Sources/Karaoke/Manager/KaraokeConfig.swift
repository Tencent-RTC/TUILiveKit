//
//  KaraokeConfig.swift
//  Pods
//
//  Created by ssc on 2025/9/12.
//

public class KaraokeConfig {
    public static let shared = KaraokeConfig()

    var SDKAPPID: Int32
    var SECRETKEY: String

    private init() {
        self.SDKAPPID = 0
        self.SECRETKEY = ""
    }

    public func updateConfig(SDKAPPID: Int32, SECRETKEY: String) {
        self.SDKAPPID = SDKAPPID
        self.SECRETKEY = SECRETKEY
    }
}

