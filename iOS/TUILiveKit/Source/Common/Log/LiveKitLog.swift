//
//  LiveKitLog.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/30.
//

import Foundation
#if canImport(TXLiteAVSDK_TRTC)
    import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
    import TXLiteAVSDK_Professional
#endif

class LiveKitLog {
    private static let API = "TuikitLog"
    private static let LOG_KEY_API = "api"
    private static let LOG_KEY_PARAMS = "params"
    private static let LOG_KEY_PARAMS_LEVEL = "level"
    private static let LOG_KEY_PARAMS_MESSAGE = "message"
    private static let LOG_KEY_PARAMS_FILE = "file"
    private static let LOG_KEY_PARAMS_LINE = "line"
    private static let LOG_KEY_PARAMS_MODULE = "module"
    private static let LOG_KEY_PARAMS_MODULE_VALUE = "TUILiveKit"

    private func `init`() {}
    enum LiveKitLogLevel: Int {
        case error = 2
        case warn = 1
        case info = 0
    }

    static func error(_ file: String, _ line: String, _ messages: String...) {
        log(level: .error, file: file, line: line, messages)
    }

    static func warn(_ file: String, _ line: String, _ messages: String...) {
        log(level: .warn, file: file, line: line, messages)
    }

    static func info(_ file: String, _ line: String, _ messages: String...) {
        log(level: .info, file: file, line: line, messages)
    }

    private static func log(level: LiveKitLogLevel = .info, file: String, line: String, _ messages: [String]) {
        let apiParams: [String: Any] = [
            LOG_KEY_API: API,
            LOG_KEY_PARAMS: [
                LOG_KEY_PARAMS_LEVEL: level.rawValue,
                LOG_KEY_PARAMS_MESSAGE: messages.joined(),
                LOG_KEY_PARAMS_MODULE: LOG_KEY_PARAMS_MODULE_VALUE,
                LOG_KEY_PARAMS_FILE: file,
                LOG_KEY_PARAMS_LINE: Int(line) ?? 0,
            ],
        ]
        let jsonData = try? JSONSerialization.data(withJSONObject: apiParams, options: .prettyPrinted)
        guard let jsonData = jsonData, let jsonString = String(data: jsonData, encoding: .utf8) else { return }
        TRTCCloud.sharedInstance().callExperimentalAPI(jsonString)
    }
}
