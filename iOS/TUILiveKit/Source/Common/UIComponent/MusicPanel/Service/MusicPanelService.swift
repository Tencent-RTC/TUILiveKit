//
//  MusicPanelService.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/4/28.
//

import RTCRoomEngine
import Combine

#if canImport(TXLiteAVSDK_TRTC)
    import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
    import TXLiteAVSDK_Professional
#endif

class MusicPanelEffects: Effects {
    typealias Environment = MusicPanelService
    
    let startPlayMusic = Effect<Environment>.dispatchingOne { actions, environment in
        actions.wasCreated(from: MusicPanelActions.startPlayMusic)
            .flatMap { action in
                environment.startPlayMusic(musicInfo: action.payload)
                    .map { musicPlayStatus in
                        if musicPlayStatus == .playing {
                            return MusicPanelActions.onMusicPlaySuccess()
                        } else {
                            return MusicPanelActions.onMusicPlayComplete()
                        }
                    }
                    .catch { error -> Just<Action> in
                        return Just(MusicPanelActions.onMusicPlayError(payload: error))
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let stopPlayMusic = Effect<Environment>.nonDispatching { actions, environment in
        actions.wasCreated(from: MusicPanelActions.stopPlayMusic)
            .sink { action in
                environment.stopPlayMusic(musicId: action.payload.id)
            }
    }
    
}

class MusicPanelService {
    
    enum MusicPlayStatus: Int {
        case playing = 0
        case complete = 999
    }
    
    private let engine = TUIRoomEngine.sharedInstance()
    private lazy var audioEffectManager: TXAudioEffectManager = self.engine.getTRTCCloud().getAudioEffectManager()
    @WeakLazyInjected var store: MusicPanelStoreProvider?
    
    func startPlayMusic(musicInfo: MusicInfo) -> AnyPublisher<MusicPlayStatus, InternalError> {
        return Future<MusicPlayStatus, InternalError> { [weak self] promise in
            guard let self = self else { return }
            let musicParam = TXAudioMusicParam()
            musicParam.id = musicInfo.id
            musicParam.path = musicInfo.path
            musicParam.publish = true
            musicParam.loopCount = Int.max
            self.audioEffectManager.startPlayMusic(musicParam) { code in
                if code == 0 {
                    promise(.success(.playing))
                } else {
                    promise(.failure(InternalError(error:TUIError.failed, message: .playMusicErrorString)))
                }
            } onProgress: { _, _ in
                
            } onComplete: { _ in
                promise(.success(.complete))
            }
        }.eraseToAnyPublisher()
    }
    
    func stopPlayMusic(musicId: Int32) {
        audioEffectManager.stopPlayMusic(musicId)
    }
    
    func updateMusicVolume(volume: Int) {
        audioEffectManager.setAllMusicVolume(volume)
    }
}

extension String {
    fileprivate static let playMusicErrorString = localized("live.anchor.link.music.play.error.title")
}
