//
//  ViewActions.swift
//  TUILiveKit
//
//  Created by aby on 2024/5/27.
//

enum ViewActions {
    static let key = "View.action"
    static let toastEventKey = "View.toastEvent"
    static let alertEventKey = "View.alertEvent"
    
    static let toastEvent = ActionTemplate(id: toastEventKey, payloadType: ToastInfo.self)
    static let alertEvent = ActionTemplate(id: alertEventKey, payloadType: AlertInfo.self)
    static let updateLinkStatus = ActionTemplate(id: key.appending(".updateLinkStatus"), payloadType: LinkStatus.self)
    static let updateLiveStatus = ActionTemplate(id:key.appending(".updateLiveStatus"), payloadType: LiveStatus.self)
    static let updateAutoOpenCameraOnSeated = ActionTemplate(id: key.appending(".updateAutoOpenCameraOnSeated"), payloadType: Bool.self)
}
