//
//  KaraokeManager.swift
//  Pods
//
//  Created by ssc on 2025/8/19.
//

import Foundation
#if canImport(TXLiteAVSDK_TRTC)
import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
import TXLiteAVSDK_Professional
#endif
import RTCRoomEngine

import Combine
import RTCCommon
import TUICore

public class KaraokeManager: NSObject {
    private(set) var state: ObservableState<KaraokeState>
    let kickedOutSubject = PassthroughSubject<Void, Never>()
    var karaokeState: KaraokeState { state.state }
    private var isNaturalEnd: Bool = true
    private let trtcCloud = TRTCCloud.sharedInstance()
    private let roomEngine = TUIRoomEngine.sharedInstance()
    private lazy var audioEffectManager: TXAudioEffectManager = trtcCloud.getAudioEffectManager()
    private let roomId: String
    private let config: KaraokeConfig = {
        return KaraokeConfig.shared
    }()
    private var scorePanelTimer: Timer?
    private lazy var chorusMusicPlayer: TXChorusMusicPlayer = {
        let chorusMusicPlayer = TXChorusMusicPlayer.createPlayer(with: trtcCloud, roomId: roomId, delegate: self) ?? TXChorusMusicPlayer()
        return chorusMusicPlayer
    }()

    public init(roomId: String) {
        self.roomId = roomId
        self.state = ObservableState(initialState: KaraokeState())
        super.init()
        roomEngine.addObserver(self)
        setAudioEffect()
    }

    deinit {
        releaseScorePanelTimer()
        roomEngine.removeObserver(self)
    }

    func setChorusRole(chorusRole: TXChorusRole) {
        let bgmParams = TRTCParams()
        bgmParams.sdkAppId = UInt32(config.SDKAPPID)
        bgmParams.userId = "\(roomId)_bgm";
        bgmParams.userSig = GenerateTestUserSig
            .genTestUserSig(
                SDKAPPID: config.SDKAPPID,
                SECRETKEY: config.SECRETKEY,
                identifier: bgmParams.userId
            )
        bgmParams.roomId = 0
        bgmParams.strRoomId = roomId
        bgmParams.role = .anchor
        chorusMusicPlayer.setChorusRole(chorusRole, trtcParamsForPlayer: bgmParams)
        state.update { state in
            state.chorusRole = chorusRole
        }
    }

    func loadCopyrightedMusic(musicId: String, playToken: String) {
        let params = TXChorusCopyrightedMusicParams()
        params.musicId = ""
        params.playToken = ""
        params.copyrightedLicenseKey = ""
        params.copyrightedLicenseUrl = ""

        chorusMusicPlayer.loadMusic(params)
    }

    func loadLocalDemoMusic(musicId: String, musicUrl: String, accompanyUrl: String) {
        let params = TXChorusExternalMusicParams()
        params.musicId = musicId
        params.musicUrl = musicUrl
        params.accompanyUrl = accompanyUrl
        params.isEncrypted = 0
        params.encryptBlockLength = 0

        chorusMusicPlayer.loadExternalMusic(params)
    }

    func start() {
        let musicTrackType = karaokeState.musicTrackType
        switchMusicTrack(trackType: musicTrackType)
        chorusMusicPlayer.start()
        self.state.update { state in
            state.playbackState = .start
            state.playProgress = 0
        }
    }

    func stopPlayback() {
        chorusMusicPlayer.stop()
        state.update { state in
            state.playbackState = .stop
            state.playProgress = 0
        }
    }

    func pausePlayback() {
        chorusMusicPlayer.pause()
        state.update { state in
            state.playbackState = .pause
        }
    }

    func resumePlayback() {
        chorusMusicPlayer.resume()
        state.update { state in
            state.playbackState = .resume
        }
    }

    func seek(positionMs: TimeInterval) {
        chorusMusicPlayer.seek(Int64(positionMs) / 1000)
        state.update { state in
            state.playProgress = positionMs
        }
    }

    func switchMusicTrack(trackType: TXChorusMusicTrack) {
        state.update { state in
            state.musicTrackType = trackType
        }
        chorusMusicPlayer.switch(trackType)
    }

    func setPlayoutVolume(volume: Int) {
        state.update { state in
            state.playoutVolume = volume
        }
        chorusMusicPlayer.setPlayoutVolume(Int32(volume))
    }

    func setPublishVolume(volume: Int) {
        state.update { state in
            state.publishVolume = volume
        }
        chorusMusicPlayer.setPublishVolume(Int32(volume))
    }

    func setMusicPitch(pitch: Float) {
        state.update { state in
            state.musicPitch = pitch
        }
        chorusMusicPlayer.setMusicPitch(pitch)
    }
    
    func prioritizeMusic(musicId: String) {
        state.update { state in
            if let index = state.selectedSongs.firstIndex(where: { $0.musicId == musicId }) {
                let removedSong = state.selectedSongs.remove(at: index)
                state.selectedSongs.insert(removedSong, at: 1)
            }
        }
        updateSelectedMusicList()
    }
    
    func eraseMusic(musicId: String) {
        state.update { state in
            if let index = state.selectedSongs.firstIndex(where: { $0.musicId == musicId }) {
                state.selectedSongs.remove(at: index)
            }

        }
        updateSelectedMusicList()
    }
    
    func addSong(selectedMusic: SelectedMusicInfo) {
        state.update { state in
            state.selectedSongs.append(selectedMusic)
        }

        if karaokeState.selectedSongs.count == 1 {
            if let song = karaokeState.songLibrary.first(where: { $0.musicId == karaokeState.selectedSongs[0].musicId}) {
                loadLocalDemoMusic(
                    musicId: song.musicId,
                    musicUrl: song.originalUrl,
                    accompanyUrl: song.accompanyUrl
                )
                start()
            }
        }
        updateSelectedMusicList()
    }

    func playNextMusic() {
        releaseScorePanelTimer()
        isNaturalEnd = false

        if karaokeState.playbackState == .idel {
            performplayNextMusic()
        } else {
            stopPlayback()
        }
    }

    func setVoiceEarMonitorEnable(_ enable: Bool) {
        audioEffectManager.enableVoiceEarMonitor(enable)
        state.update { state in
            state.EarMonitor = enable
        }
    }

    func enableScore(enable: Bool) {
        guard let data = try? JSONEncoder().encode(enable),
              let jsonString = String(data: data, encoding: .utf8) else {
            return
        }

        let metadata: [String: String] = ["EnableScore": jsonString]
        roomEngine.setRoomMetadataByAdmin(metadata, onSuccess: {
        }, onError: { error, message in
        })
    }
    
    func setReverbType(_ type: MusicReverbType) {
        audioEffectManager.setVoiceReverbType(TXVoiceReverbType(rawValue: type.rawValue) ?? ._0)
        state.update { state in
            state.reverbType = type
        }
    }

    public func synchronizeMetadata(isOwner: Bool) {
        if !isOwner {
            getMetadata()
        } else {
            updateSelectedMusicList()
            enableScore(enable: false)
        }
    }

    public func exit() {
        state.update { state in
            state.enableRequestMusic = false
            state.selectedSongs = []
        }
        if karaokeState.chorusRole != .leadSinger {
            return
        }
        guard let data = try? JSONEncoder().encode(""),
              let jsonString = String(data: data, encoding: .utf8) else {
            return
        }

        let metadata: [String: String] = ["SongPlayList": jsonString]
        roomEngine.setRoomMetadataByAdmin(metadata, onSuccess: {
        }, onError: { error, message in
        })

        guard let data = try? JSONEncoder().encode(false),
              let jsonString = String(data: data, encoding: .utf8) else {
            return
        }

        let metadata1: [String: String] = ["EnableRequestMusic": jsonString]
        roomEngine.setRoomMetadataByAdmin(metadata1, onSuccess: {
        }, onError: { error, message in
        })

        enableScore(enable: false)
        stopPlayback()
    }

    public func show() {
        if karaokeState.chorusRole != .leadSinger {
            return
        }
        state.update { state in
            state.enableRequestMusic = true
        }
        guard let data = try? JSONEncoder().encode(true),
              let jsonString = String(data: data, encoding: .utf8) else {
            return
        }

        let metadata: [String: String] = ["EnableRequestMusic": jsonString]
        roomEngine.setRoomMetadataByAdmin(metadata, onSuccess: {
        }, onError: { error, message in
        })
    }

    private func updateSelectedMusicList() {
        guard let data = try? JSONEncoder().encode(karaokeState.selectedSongs),
              let jsonString = String(data: data, encoding: .utf8) else {
            return
        }

        let metadata: [String: String] = ["SongPlayList": jsonString]
        roomEngine.setRoomMetadataByAdmin(metadata, onSuccess: {
        }, onError: { error, message in
        })
    }

    private func performplayNextMusic() {
        state.update { state in
            state.playbackState = .stop
            isNaturalEnd = true
        }
        if karaokeState.selectedSongs.isEmpty {
            return
        }
        state.update { state in
            state.selectedSongs.remove(at: 0)
        }
        updateSelectedMusicList()

        if karaokeState.selectedSongs.isEmpty { return }
        if let song = karaokeState.songLibrary.first(where: { $0.musicId == karaokeState.selectedSongs[0].musicId}) {
            loadLocalDemoMusic(musicId: song.musicId, musicUrl: song.originalUrl, accompanyUrl: song.accompanyUrl)
            start()
        }
    }

    private func getMetadata() {
        roomEngine.getRoomMetadata(["SongPlayList"], onSuccess: { [weak self] response in
            guard let self = self, let value = response["SongPlayList"] else { return }
            parsePlayListJson(value)
        }, onError: { error, message in
        })

        roomEngine.getRoomMetadata(["EnableSocre"], onSuccess: { [weak self] response in
            guard let self = self, let value = response["EnableSocre"] else { return }
            parseEnableScoreJson(value)
        }, onError: { error, message in
        })

        roomEngine.getRoomMetadata(["EnableRequestMusic"], onSuccess: { [weak self] response in
            guard let self = self, let value = response["EnableRequestMusic"] else { return }
            parseenableRequestMusicJson(value)
        }, onError: { error, message in
        })
    }

    private func releaseScorePanelTimer() {
        scorePanelTimer?.invalidate()
        scorePanelTimer = nil
    }

    func subscribe<Value>(_ selector: StateSelector<KaraokeState, Value>) -> AnyPublisher<Value, Never> {
        return state.subscribe(selector)
    }
}

extension KaraokeManager: ITXChorusPlayerDelegate {
    public func onChorusMusicLoadSucceed(_ musicId: String,
                                  lyricList: [TXLyricLine],
                                  pitchList: [TXReferencePitch]) {
        state.update { state in
            state.currentMusicId = musicId
        }
    }

    public func onVoicePitchUpdated(_ pitch: Int32, hasVoice: Bool, progressMs: Int64) {
        state.update { state in
            state.pitch = pitch
        }
    }

    public func onChorusRequireLoadMusic(_ musicId: String) {
        releaseScorePanelTimer()
        loadLocalDemoMusic(
            musicId: musicId,
            musicUrl: karaokeState.songLibrary
                .first{$0.musicId == musicId}?.originalUrl ?? "",
            accompanyUrl: karaokeState.songLibrary
                .first{$0.musicId == musicId}?.accompanyUrl ?? ""
        )
        state.update { state in
            state.currentMusicId = musicId
        }
    }

    public func onMusicProgressUpdated(_ progressMs: Int64, durationMs: Int64) {
        state.update { state in
            state.playProgress = TimeInterval(progressMs) / 1000.0
            state.currentMusicTotalDuration = TimeInterval(durationMs) / 1000.0
            if state.currentMusicTotalDuration - state.playProgress <= 1 || state.playProgress > state.currentMusicTotalDuration {
                isNaturalEnd = true
            } else {
                isNaturalEnd = false
            }
        }
    }

    public func onChorusError(_ errCode: TXChorusError, errMsg: String) {
    }

    public func onChorusMusicLoadProgress(_ musicId: String, progress: Float) {
    }

    public func onVoiceScoreUpdated(_ currentScore: Int32, averageScore: Int32, currentLine: Int32) {
        state.update { state in
            state.currentScore = currentScore
            state.averageScore = averageScore
        }
    }

    public func onChorusStopped() {
        if self.karaokeState.chorusRole == .leadSinger {
            setReverb(enable: false)
        }
        if isNaturalEnd && karaokeState.enableScore {
            state.update { state in
                state.playbackState = .idel
            }
            scorePanelTimer = Timer.scheduledTimer(withTimeInterval: 5.0, repeats: false) { [weak self] _ in
                guard let self = self else {return}
                if self.karaokeState.chorusRole == .leadSinger {
                    self.performplayNextMusic()
                } else {
                    state.update { state in
                        state.playbackState = .stop
                    }
                }
            }
        } else {
            if karaokeState.chorusRole == .leadSinger {
                self.performplayNextMusic()
            } else {
                state.update { state in
                    state.playbackState = .stop
                }
            }
        }
    }

    public func onChorusStarted() {
        if self.karaokeState.chorusRole == .leadSinger {
            setReverb(enable: true)
        }
        state.update { state in
            state.playbackState = .start
        }
        getMetadata()
    }

    public func onChorusPaused() {
        state.update { state in
            state.playbackState = .pause
        }
    }

    public func onChorusResumed() {
        state.update { state in
            state.playbackState = .resume
        }
    }
}

extension KaraokeManager: TUIRoomObserver {
    public func onRoomMetadataChanged(key: String, value: String) {
        if key == "SongPlayList" && !value.isEmpty {
            parsePlayListJson(value)
        } else if key == "EnableScore" && !value.isEmpty {
            parseEnableScoreJson(value)
        } else if key == "EnableRequestMusic" && !value.isEmpty {
            parseenableRequestMusicJson(value)
        }
    }

    public func onRoomDismissed(roomId: String, reason: TUIRoomDismissedReason) {
        kickedOutSubject.send()
    }

    public func onKickedOutOfRoom(roomId: String, reason: TUIKickedOutOfRoomReason, message: String) {
        kickedOutSubject.send()
    }

    private func parsePlayListJson(_ jsonString: String) {
        guard !jsonString.isEmpty else { return }
        guard let jsonData = jsonString.data(using: .utf8) else { return }
        do {
            let songs = try JSONDecoder().decode([SelectedMusicInfo].self, from: jsonData)
            guard songs != karaokeState.selectedSongs else { return }
            state.update { state in
                state.selectedSongs = songs
                if state.selectedSongs.isEmpty {
                    state.currentMusicId = ""
                    state.currentMusicTotalDuration = 0
                }
            }
        } catch {
        }
    }

    private func parseEnableScoreJson(_ jsonString: String) {
        guard !jsonString.isEmpty else { return }
        guard let data = jsonString.data(using: .utf8),
              let isEnabled = try? JSONDecoder().decode(Bool.self, from: data) else {
            return
        }
        state.update { state in
            state.enableScore = isEnabled
        }
    }

    private func parseenableRequestMusicJson(_ jsonString: String) {
        guard !jsonString.isEmpty else { return }
        guard let data = jsonString.data(using: .utf8),
              let isEnabled = try? JSONDecoder().decode(Bool.self, from: data) else {
            return
        }
        state.update { state in
            state.enableRequestMusic = isEnabled
        }
    }
}

extension KaraokeManager {
    func setAudioEffect() {
        let dspConfig: [String: Any] = [
            "api": "setPrivateConfig",
            "params": [
                "configs": [
                    [
                        "key": "Liteav.Audio.common.dsp.version",
                        "value": "2",
                        "default": "1"
                    ]
                ]
            ]
        ]
        if let jsonData = try? JSONSerialization.data(withJSONObject: dspConfig),
           let jsonString = String(data: jsonData, encoding: .utf8) {
            trtcCloud.callExperimentalAPI(jsonString)
        }

        let hifiConfig: [String: Any] = [
            "api": "setPrivateConfig",
            "params": [
                "configs": [
                    [
                        "key": "Liteav.Audio.common.smart.3a.strategy.flag",
                        "value": "16",
                        "default": "1"
                    ]
                ]
            ]
        ]
        if let jsonData = try? JSONSerialization.data(withJSONObject: hifiConfig),
           let jsonString = String(data: jsonData, encoding: .utf8) {
            trtcCloud.callExperimentalAPI(jsonString)
        }

        let aiecModelConfig: [String: Any] = [
            "api": "setPrivateConfig",
            "params": [
                "configs": [
                    [
                        "key": "Liteav.Audio.common.ai.ec.model.type",
                        "value": "2",
                        "default": "2"
                    ]
                ]
            ]
        ]
        if let jsonData = try? JSONSerialization.data(withJSONObject: aiecModelConfig),
           let jsonString = String(data: jsonData, encoding: .utf8) {
            trtcCloud.callExperimentalAPI(jsonString)
        }

        let enableAiecConfig: [String: Any] = [
            "api": "setPrivateConfig",
            "params": [
                "configs": [
                    [
                        "key": "Liteav.Audio.common.enable.ai.ec.module",
                        "value": "1",
                        "default": "1"
                    ]
                ]
            ]
        ]
        if let jsonData = try? JSONSerialization.data(withJSONObject: enableAiecConfig),
           let jsonString = String(data: jsonData, encoding: .utf8) {
            trtcCloud.callExperimentalAPI(jsonString)
        }

        let enableAiModuleConfig: [String: Any] = [
            "api": "setPrivateConfig",
            "params": [
                "configs": [
                    [
                        "key": "Liteav.Audio.common.ai.module.enabled",
                        "value": "1",
                        "default": "1"
                    ]
                ]
            ]
        ]
        if let jsonData = try? JSONSerialization.data(withJSONObject: enableAiModuleConfig),
           let jsonString = String(data: jsonData, encoding: .utf8) {
            trtcCloud.callExperimentalAPI(jsonString)
        }
    }

    func setReverb(enable: Bool) {
        let customReverbParams: [String: Any] = [
            "enable": enable,
            "RoomSize": 60,
            "PreDelay": 20,
            "Reverberance": 40,
            "Damping": 50,
            "ToneLow": 30,
            "ToneHigh": 100,
            "WetGain": -3,
            "DryGain": 0,
            "StereoWidth": 40,
            "WetOnly": false
        ]
        let customReverbConfig: [String: Any] = [
            "api": "setCustomReverbParams",
            "params": customReverbParams
        ]
        if let jsonData = try? JSONSerialization.data(withJSONObject: customReverbConfig),
           let jsonString = String(data: jsonData, encoding: .utf8) {
            trtcCloud.callExperimentalAPI(jsonString)
        }
    }

}
