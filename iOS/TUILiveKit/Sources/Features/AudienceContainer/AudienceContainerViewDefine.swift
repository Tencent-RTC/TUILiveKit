//
//  AudienceContainerViewDefine.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2025/6/20.
//

import RTCRoomEngine

public protocol AudienceContainerViewDelegate: AnyObject {
    func onClickFloatWindow()
    func onLiveEnded(roomId: String, avatarUrl: String, userName: String)
}

public protocol AudienceContainerViewDataSource: AnyObject {
    typealias LiveListSuccessBlock = (String, [TUILiveInfo]) -> Void
    typealias LiveListErrorBlock = (Int, String) -> Void
    func fetchLiveList(cursor: String, onSuccess: @escaping LiveListSuccessBlock, onError: @escaping LiveListErrorBlock)
}
