//
//  ErrorActions.swift
//  TUILivekit
//
//  Created by aby on 2024/4/9.
//

import Foundation

enum ErrorActions {
    static let key = "Error.action"
    static let throwError = ActionTemplate(id: key.appending(".throwError"), payloadType: ErrorService.OperateError.self)
}
