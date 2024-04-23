//
//  RoomConfig.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/10.
//

import Foundation

class RoomConfig {
    init() {}
    let enableLink: Observable<Bool> = Observable(true)
    let enablePk: Observable<Bool> = Observable(true)
    let enableGift: Observable<Bool> = Observable(true)
    let enableLike: Observable<Bool> = Observable(true)
    let enableChat: Observable<Bool> = Observable(true)
    let enableHiddenNickname: Observable<Bool> = Observable(false)
    let enableShare: Observable<Bool> = Observable(false)
}

extension RoomConfig {
    func getListConfigModel() -> [MoreSettingsModel] {
        return [MoreSettingsModel(title: .linkText, enable: enableLink),
                MoreSettingsModel(title: .pkText, enable: enablePk),
                MoreSettingsModel(title: .giftText, enable: enableGift),
                MoreSettingsModel(title: .likeText, enable: enableLike),
                MoreSettingsModel(title: .chatText, enable: enableChat),
                MoreSettingsModel(title: .nicknameText, enable: enableHiddenNickname),
                MoreSettingsModel(title: .shareText, enable: enableShare),]
    }
}

private extension String {
    static var linkText = {
        localized("live.anchor.more.set.link")
    }()

    static var pkText = {
        localized("live.anchor.more.set.pk")
    }()

    static var giftText = {
        localized("live.anchor.more.set.gift")
    }()

    static var likeText = {
        localized("live.anchor.more.set.like")
    }()

    static var chatText = {
        localized("live.anchor.more.set.chat")
    }()

    static var nicknameText = {
        localized("live.anchor.more.set.nickname")
    }()

    static var shareText = {
        localized("live.anchor.more.set.share")
    }()
}
