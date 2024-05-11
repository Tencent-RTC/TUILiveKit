//
//  MusicPanelActions.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/4/28.
//

enum MusicPanelActions {
    static let key = "MusicPanel.action"
    static let startPlayMusic = ActionTemplate(id: key.appending(".startPlayMusic"), payloadType: MusicInfo.self)
    static let stopPlayMusic = ActionTemplate(id: key.appending(".stopPlayMusic"), payloadType: MusicInfo.self)
    static let deleteMusic = ActionTemplate(id: key.appending(".deleteMusic"), payloadType: MusicInfo.self)
    
    static let onMusicPlaySuccess = ActionTemplate(id: key.appending(".onMusicPlaySuccess"))
    static let onMusicPlayError = ActionTemplate(id: key.appending(".onMusicPlayError"), payloadType: InternalError.self)
    static let onMusicPlayComplete = ActionTemplate(id: key.appending(".onMusicPlayComplete"))
}

