//
//  ViewActions.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/6.
//

// Action
enum NavigatorActions {
    static let key = "Navigator.action"
    static let navigatorTo = ActionTemplate(id: "Navigator.action.present", payloadType: NavigationState.Router.self)
}

enum ViewActions {
    static let key = "View.action"
    static let subjectKey = "View.subject"
    static let customEventKey = "View.customEvent"
    static let startLoading = ActionTemplate(id: key.appending(".startLoading"))
    static let endLoading = ActionTemplate(id: key.appending("endLoading"))
    
    static let updateBottomMenus = ActionTemplate(id: key.appending(".updateBottomMenu"))
    
    static let toast = ActionTemplate(id: subjectKey.appending(".toast"), payloadType: ToastInfo.self)
    static let like = ActionTemplate(id: customEventKey.appending(".like"))
    static let endView = ActionTemplate(id: customEventKey.appending(".endView"))
}
