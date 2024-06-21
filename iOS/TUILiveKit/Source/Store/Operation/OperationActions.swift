//
//  OperationActions.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/6/21.
//

import RTCRoomEngine

enum OperationActions {
    static let key = "Operation.action"
    static let clearAllState =  ActionTemplate(id: key.appending(".clearAllState"))
}
