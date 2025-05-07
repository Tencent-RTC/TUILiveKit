//
//  ToastInfo.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/11.
//

import Foundation

struct ToastInfo: Identifiable {
    enum Position {
        case center
        case bottom
    }
    let id: UUID
    let duration: TimeInterval
    let position: Position
    let message: String
    
    init(message: String, position: Position = .bottom, duration: TimeInterval = 1.5) {
        id = UUID()
        self.message = message
        self.position = position
        self.duration = duration
    }
}

extension ToastInfo: Equatable {
    static func ==(lhs: ToastInfo, rhs: ToastInfo) -> Bool{
        return lhs.id == rhs.id || lhs.message == rhs.message
    }
}
