//
//  LiveKitLog.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/30.
//

import Foundation
#if TXLiteAVSDK_TRTC
    import TXLiteAVSDK_TRTC
#elseif TXLiteAVSDK_Professional
    import TXLiteAVSDK_Professional
#endif

class LiveKitLog {
    private func `init`() {}
    enum LiveKitLogLevel {
        case error
        case warn
        case info
        case debug
        func getString() -> String {
            switch self {
            case .error:
                return "LiveKitLog Error"
            case .warn:
                return "LiveKitLog Warn"
            case .info:
                return "LiveKitLog Info"
            case .debug:
                return "LiveKitLog Debug"
            }
        }
    }

    static func error(_ file: String, _ line: String, _ messages: String...) {
        log(level: .error, file: file, line: line, messages)
    }

    static func warn(_ file: String,_ line: String , _ messages: String...) {
        log(level: .warn, file: file, line: line, messages)
    }

    static func info(_ file: String,_ line: String, _ messages: String...) {
        log(level: .info, file: file, line: line, messages)
    }

    static func debug(_ file: String,_ line: String, _ messages: String...) {
        log(level: .debug, file: file, line: line, messages)
    }

    private static func log(level: LiveKitLogLevel = .info, file: String, line: String, _ messages: [String]) {
        var logs = level.getString()
        logs.append(" [\(URL(fileURLWithPath: file).lastPathComponent),\(line)] ")
        for message in messages {
            logs.append(message)
        }
        if level == .debug {
            debugPrint(logs)
        } else {
            let cloud = TRTCCloud.sharedInstance()
            let selector = NSSelectorFromString("apiLog:")
            if cloud.responds(to: selector) {
                cloud.perform(selector, with: logs)
            }
        }
    }
}
