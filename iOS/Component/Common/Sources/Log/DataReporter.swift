//
//  DataReporter.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/6/13.
//

import Foundation
import RTCRoomEngine

public class DataReporter {
    
    public enum ComponentType: Int {
        case liveRoom = 21
        case voiceRoom = 22
    }
    
    private static let framework: Int = 1
    private static let language: Int = 3
    public static var componentType: ComponentType = .liveRoom
    static func reportFramework() {
        let apiParams: [String : Any] = [
            "api": "setFramework",
            "params": [
              "framework": framework,
              "component": componentType.rawValue,
              "language": language,
            ],
          ]
        callExperimentalAPI(params: apiParams)
    }
    
    public static func reportEventData(eventKey: Int) {
        let apiParams: [String: Any] = [
            "api": "KeyMetricsStats",
            "params": [
                "key": eventKey,
            ],
        ]
        callExperimentalAPI(params: apiParams)
    }
    
    private static func callExperimentalAPI(params: [String : Any]) {
        do {
            let jsonData = try JSONSerialization.data(withJSONObject: params, options: .prettyPrinted)
            if let jsonString = String(data: jsonData, encoding: .utf8) {
                print(jsonString)
                TUIRoomEngine.callExperimentalAPI(jsonStr: jsonString)
            } else {
                print("Error converting JSON data to string")
            }
        } catch {
            print("Error converting dictionary to JSON: \(error.localizedDescription)")
        }
    }
}
