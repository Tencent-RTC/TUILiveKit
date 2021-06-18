//
//  TRTCLiveRoomSoundEffectViewModel.swift
//  TXLiteAVDemo
//
//  Created by gg on 2021/3/30.
//  Copyright © 2021 Tencent. All rights reserved.
//

import Foundation

@objcMembers
public class TRTCLiveRoomAudioEffectCellModel: NSObject {
    public var actionID: Int = 0
    public var title: String = ""
    public var icon: UIImage?
    public var selectIcon: UIImage?
    public var selected: Bool = false
    public var action: (()->())?
}

@objcMembers
public class TRTCLiveRoomMusicModel: NSObject {
    public var musicID : Int32 = 0
    public var musicName : String = ""
    public var singerName : String = ""
    public var isLocal : Bool = true
    public var resourceUrl : String = ""
    public var action : ((_ isSelected :Bool, _ model: TRTCLiveRoomMusicModel)->())?
    
    public var currentTime: Int = 0
    public var totalTime: Int = 0
}

public protocol TRTCLiveRoomSoundEffectViewResponder: class {
    func bgmOnPrepareToPlay()
    func bgmOnPlaying(current: Int, total: Int)
    func bgmOnCompletePlaying()
}

@objcMembers
public class TRTCLiveRoomSoundEffectViewModel: NSObject {
    
    public weak var viewResponder: TRTCLiveRoomSoundEffectViewResponder?
    
    public let room: TRTCLiveRoom
    public init(liveRoom: TRTCLiveRoom) {
        self.room = liveRoom
        super.init()
    }
    
    public lazy var manager : TXAudioEffectManager? = {
        return room.getAudioEffectManager()
    }()
    
    public var currentMusicVolum: Int = 100
    public var currentVocalVolume: Int = 100
    public var currentPitchVolum: Double = 0
    
    public var bgmID : Int32 = 0
    
    public func setVolume(music: Int) {
        currentMusicVolum = music
        guard let manager = manager else {
            return
        }
        if bgmID != 0 {
            manager.setMusicPlayoutVolume(bgmID, volume: music)
            manager.setMusicPublishVolume(bgmID, volume: music)
        }
    }
    
    public func setEarMonitor(_ enable: Bool) {
        guard let manager = manager else {
            return
        }
        manager.enableVoiceEarMonitor(enable)
    }
    
    public func setVolume(person: Int) {
        currentVocalVolume = person
        guard let manager = manager else {
            return
        }
        manager.setVoiceVolume(person)
    }
    
    public func setPitch(person: Double) {
        currentPitchVolum = person
        guard let manager = manager else {
            return
        }
        if bgmID != 0 {
            manager.setMusicPitch(bgmID, pitch: person)
        }
    }
    
    // MARK: - BGM
    public var isPlaying = false
    public var isPlayingComplete = false
    public var currentPlayingModel: TRTCLiveRoomMusicModel?
    public lazy var bgmDataSource: [TRTCLiveRoomMusicModel] = {
        var res : [TRTCLiveRoomMusicModel] = []
        let urls = [
            "https://sdk-liteav-1252463788.cos.ap-hongkong.myqcloud.com/app/res/bgm/trtc/PositiveHappyAdvertising.mp3",
            "https://sdk-liteav-1252463788.cos.ap-hongkong.myqcloud.com/app/res/bgm/trtc/SadCinematicPiano.mp3",
            "https://sdk-liteav-1252463788.cos.ap-hongkong.myqcloud.com/app/res/bgm/trtc/WonderWorld.mp3"]
        let names : [String] = [
            .musicTitle1Text,
            .musicTitle2Text,
            .musicTitle3Text,
        ]
        for i in 0..<3 {
            let model = TRTCLiveRoomMusicModel()
            model.musicID = Int32(1000 + i)
            model.musicName = names[i]
            model.resourceUrl = urls[i]
            model.action = { [weak self] (isSelected, model) in
                guard let `self` = self else { return }
                self.stopPlay()
                self.playMusic(model)
            }
            res.append(model)
        }
        return res
    }()
    
    public func playMusic(_ model: TRTCLiveRoomMusicModel) {
        
        guard let manager = manager else {
            return
        }
        if bgmID == model.musicID {
            resumePlay()
            return
        }
        else {
            stopPlay()
        }
        currentPlayingModel = model
        bgmID = model.musicID
        let param = TXAudioMusicParam()
        param.id = bgmID
        param.path = model.resourceUrl
        param.loopCount = 0
        manager.startPlayMusic(param) { [weak self] (errCode) in
            guard let `self` = self else { return }
            DispatchQueue.main.async {
                self.isPlaying = true
                self.isPlayingComplete = false
                self.viewResponder?.bgmOnPrepareToPlay()
            }
        } onProgress: { [weak self] (progress, duration) in
            guard let `self` = self else { return }
            DispatchQueue.main.async {
                let current = progress/1000
                let total = duration/1000
                if let model = self.currentPlayingModel {
                    model.currentTime = current
                    model.totalTime = total
                }
                self.viewResponder?.bgmOnPlaying(current: current, total: total)
            }
        } onComplete: { [weak self] (errCode) in
            guard let `self` = self else { return }
            DispatchQueue.main.async {
                self.bgmID = 0
                self.isPlaying = false
                self.isPlayingComplete = true
                self.viewResponder?.bgmOnCompletePlaying()
            }
        }
    }
    
    public func stopPlay() {
        isPlaying = false
        guard let manager = manager else {
            return
        }
        if bgmID != 0 {
            manager.stopPlayMusic(bgmID)
            currentPlayingModel = nil
            bgmID = 0
        }
    }
    
    public func pausePlay() {
        isPlaying = false
        guard let manager = manager else {
            return
        }
        if bgmID != 0 {
            manager.pausePlayMusic(bgmID)
        }
    }
    
    public func resumePlay() {
        isPlaying = true
        guard let manager = manager else {
            return
        }
        if bgmID != 0 {
            manager.resumePlayMusic(bgmID)
        }
    }
    
    public func clearStatus() {
        currentPlayingModel = nil
        if bgmID != 0 {
            setPitch(person: 0)
            stopPlay()
            bgmID = 0
        }
        setVolume(music: 100)
        setVolume(person: 100)
        
    }
    
    
    // MARK: - Voice change and reverb
    public var currentChangerType : TXVoiceChangeType = ._0
    public var currentReverb : TXVoiceReverbType = ._0
    
    public lazy var reverbDataSource: [TRTCLiveRoomAudioEffectCellModel] = {
        var res: [TRTCLiveRoomAudioEffectCellModel] = []
        let titleArray = [
            LiveRoomLocalize("ASKit.MenuItem.No effect"),
            LiveRoomLocalize("ASKit.MenuItem.Karaoke room"),
            LiveRoomLocalize("ASKit.MenuItem.Metallic"),
            LiveRoomLocalize("ASKit.MenuItem.Deep"),
            LiveRoomLocalize("ASKit.MenuItem.Resonant"),
            ]
        let iconNameArray = [
            "originState_nor",
            "Reverb_KTV_nor",
            "Reverb_jinshu_nor",
            "Reverb_dichen_nor",
            "Reverb_hongliang_nor",
        ]
        let iconSelectedNameArray = [
            "originState_sel",
            "Reverb_KTV_sel",
            "Reverb_jinshu_sel",
            "Reverb_dichen_sel",
            "Reverb_hongliang_sel",
        ]
        for index in 0..<titleArray.count {
            let title = titleArray[index]
            let normalIconName = iconNameArray[index]
            let selectIconName = iconSelectedNameArray[index]
            
            let model = TRTCLiveRoomAudioEffectCellModel()
            model.actionID = index
            model.title = title
            model.selected = title == LiveRoomLocalize("ASKit.MenuItem.No effect")
            model.icon = UIImage.init(named: normalIconName, in: LiveRoomBundle(), compatibleWith: nil)
            model.selectIcon = UIImage.init(named: selectIconName, in: LiveRoomBundle(), compatibleWith: nil)
            model.action = { [weak self] in
                guard let `self` = self else { return }
                let type = self.switch2ReverbType(index)
                self.manager?.setVoiceReverbType(type)
                self.currentReverb = type
            }
            if model.icon != nil {
                res.append(model)
            }
        }
        return res
    }()
    
    public lazy var voiceChangeDataSource: [TRTCLiveRoomAudioEffectCellModel] = {
        var res: [TRTCLiveRoomAudioEffectCellModel] = []
        
        let titleArray =
            [LiveRoomLocalize("ASKit.MenuItem.Original"),
             LiveRoomLocalize("ASKit.MenuItem.Naughty boy"),
             LiveRoomLocalize("ASKit.MenuItem.Little girl"),
             LiveRoomLocalize("ASKit.MenuItem.Middle-aged man"),
             LiveRoomLocalize("ASKit.MenuItem.Ethereal voice"),
             ]
        
        let iconNameArray = [
            "originState_nor",
            "voiceChange_xionghaizi_nor",
            "voiceChange_loli_nor",
            "voiceChange_dashu_nor",
            "voiceChange_kongling_nor",
        ]
        
        let iconSelectedNameArray = [
            "originState_sel",
            "voiceChange_xionghaizi_sel",
            "voiceChange_loli_sel",
            "voiceChange_dashu_sel",
            "voiceChange_kongling_sel",
            ]
        
        for index in 0..<titleArray.count {
            let title = titleArray[index]
            let normalIconName = iconNameArray[index]
            let selectedIconName = iconSelectedNameArray[index]
            let model = TRTCLiveRoomAudioEffectCellModel()
            model.title = title
            model.actionID = index
            model.selected = title == LiveRoomLocalize("ASKit.MenuItem.Original")
            model.icon = UIImage.init(named: normalIconName, in: LiveRoomBundle(), compatibleWith: nil)
            model.selectIcon = UIImage.init(named: selectedIconName, in: LiveRoomBundle(), compatibleWith: nil)
            model.action = { [weak self] in
                guard let `self` = self else { return }
                let type = self.switch2VoiceChangeType(index)
                self.manager?.setVoiceChangerType(type)
                self.currentChangerType = type
            }
            if model.icon != nil {
                res.append(model)
            }
        }
        return res
    }()
    
    public func switch2VoiceChangeType(_ index: Int) -> TXVoiceChangeType {
        switch index {
        case 0:
            return ._0
        case 1:
            return ._1
        case 2:
            return ._2
        case 3:
            return ._3
        case 4:
            return ._11
        default:
            return ._0
        }
    }
    
    public func switch2ReverbType(_ index: Int) -> TXVoiceReverbType {
        switch index {
        case 0:
            return ._0
        case 1:
            return ._1
        case 2:
            return ._6
        case 3:
            return ._4
        case 4:
            return ._5
        default:
            return ._0
        }
    }
}

/// MARK: - internationalization string
fileprivate extension String {
    static let musicTitle1Text = LiveRoomLocalize("Demo.TRTC.VoiceRoom.musicname1")
    static let musicTitle2Text = LiveRoomLocalize("Demo.TRTC.VoiceRoom.musicname2")
    static let musicTitle3Text = LiveRoomLocalize("Demo.TRTC.VoiceRoom.musicname3")
}
