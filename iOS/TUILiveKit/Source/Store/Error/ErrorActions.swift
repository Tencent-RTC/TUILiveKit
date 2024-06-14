//
//  ErrorActions.swift
//  TUILivekit
//
//  Created by aby on 2024/4/9.
//

import Foundation


enum ErrorActions {
    static let key = "Error.action"
    // If an error occurs and it is necessary to notify the UI layer for processing, throw it through the action defined here.
    static let throwError = ActionTemplate(id: key.appending(".throwError"), payloadType: ErrorService.OperateError.self)
}
