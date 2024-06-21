//
//  LiveListActions.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/5/30.
//

import RTCRoomEngine

enum LiveListNavigatorActions {
    static let key = "LiveList.navigation.Action"
    static let navigatorTo = ActionTemplate(id: key.appending(".present"), payloadType: LiveListNavigationState.Router.self)
}

enum LiveListActions {
    static let key = "LiveList.Action"
    static let getLiveInfoList = ActionTemplate(id: key.appending(".getLiveInfoList"), payloadType: String.self)
    static let updateLiveInfoList = ActionTemplate(id: key.appending(".updateLiveInfoList"), payloadType: LiveListResult.self)
    static let toastError = ActionTemplate(id: key.appending("toastError"), payloadType: InternalError.self)
}
