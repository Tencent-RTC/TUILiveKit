//
//  MusicPanelSelectors.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/4/28.
//

enum MusicPanelSelectors {
    static let getCurrentPlayMusic = Selector(keyPath: \MusicPanelState.currentPlayMusic)
    static let getMusicInfoList = Selector(keyPath: \MusicPanelState.musicInfoList)
    static let getIsPlaying = Selector.with(getCurrentPlayMusic, projector: \.?.isPlaying)
}

