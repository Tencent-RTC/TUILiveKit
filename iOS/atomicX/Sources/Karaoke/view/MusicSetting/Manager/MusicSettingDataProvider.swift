//
//  MusicSettingDataProvider.swift
//  TUILiveKit
//
//  Created by aby on 2024/4/3.
//
import Combine
import RTCCommon
#if canImport(TXLiteAVSDK_TRTC)
import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
import TXLiteAVSDK_Professional
#endif

protocol MusicSettingMenuDateGenerator {
    typealias Section = Int
    var MusicSettingMenus: [Section: [MusicSettingItem]] { get }
    var MusicSettingSectionTitles: [Section: String] { get }
}

class MusicSettingDataProvider {
    
    private weak var manager: KaraokeManager?
    init(manager: KaraokeManager) {
        self.manager = manager
    }
}

extension MusicSettingDataProvider: MusicSettingMenuDateGenerator {
    
    var MusicSettingMenus: [Section : [MusicSettingItem]] {
        return generateMusicSettingData()
    }
    
    var MusicSettingSectionTitles: [Section : String] {
        return [
            0: "",
            1: .audioSetting,
        ]
    }
    
    func generateMusicSettingData() -> [Section : [MusicSettingItem]] {
        var menus: [Int:[MusicSettingItem]] = [:]
        menus[0] = firstSectionMenus()
        menus[1] = secondSectionMenus()
        return menus
    }
    
    private func firstSectionMenus() -> [MusicSettingItem] {
        guard let manager = manager else { return [] }
        var firstSection: [MusicSettingItem] = []

        var original = MusicSwitchItem(
            title: .Original,
            isOn: manager.karaokeState.musicTrackType == .originalSong)
        original.action = { [weak self] isOpened in
            guard let self = self else { return }
            self.manager?
                .switchMusicTrack(
                    trackType: isOpened ? .originalSong : .accompaniment
                )
        }

        original.subscribeState = { [weak self] cell, cancellableSet in
            guard let self = self else { return }
            self.manager?.subscribe(StateSelector(keyPath: \.musicTrackType))
                .receive(on: DispatchQueue.main)
                .sink { [weak cell] musicTrackType in
                    if musicTrackType == .accompaniment {
                        cell?.isSelected = false
                    } else {
                        cell?.isSelected = true
                    }
                }
                .store(in: &cancellableSet)
        }
        firstSection.append(original)

        var enableScore = MusicSwitchItem(
            title: .score,
            isOn: manager.karaokeState.enableScore
        )
        enableScore.action = { [weak self] isOpened in
            guard let self = self else { return }
            self.manager?.enableScore(enable: isOpened)
        }
        enableScore.subscribeState = { [weak self] cell, cancellableSet in
            guard let self = self else { return }
            self.manager?.subscribe(StateSelector(keyPath: \.enableScore))
                .receive(on: DispatchQueue.main)
                .sink { [weak cell] enableScore in
                    if !enableScore {
                        cell?.isSelected = false
                    } else {
                        cell?.isSelected = true
                    }
                }
                .store(in: &cancellableSet)
        }
        firstSection.append(enableScore)
        return firstSection
    }
    
    private func secondSectionMenus() -> [MusicSettingItem] {
        guard let manager = manager else { return [] }
        var secondSection:[MusicSettingItem] = []
        var microphoneVolume = MusicSliderItem(title: .captureVolume)
        microphoneVolume.min = 0
        microphoneVolume.max = 100
        microphoneVolume.currentValue = Float(manager.karaokeState.publishVolume)
        microphoneVolume.valueChanged = { [weak self] value in
            guard let self = self else { return }
            self.manager?.setPublishVolume(volume: Int(value))
        }
        microphoneVolume.subscribeState = { [weak self] cell, cancellableSet in
            guard let self = self else { return }
            self.manager?.subscribe(StateSelector(keyPath: \.publishVolume))
                .receive(on: DispatchQueue.main)
                .sink { [weak cell] value in
                    guard let sliderCell = cell else { return }
                    sliderCell.valueLabel.text = "\(value)"
                    sliderCell.configSlider.value = Float(value)
                }
                .store(in: &cancellableSet)
        }
        secondSection.append(microphoneVolume)

        var musicVolume = MusicSliderItem(title: .playoutVolume)
        musicVolume.min = 0
        musicVolume.max = 100
        musicVolume.currentValue = Float(manager.karaokeState.playoutVolume)
        musicVolume.valueChanged = { [weak self] value in
            self?.manager?.setPlayoutVolume(volume: Int(value))
        }
        musicVolume.subscribeState = { [weak self] cell, cancellableSet in
            guard let self = self else { return }
            self.manager?.subscribe(StateSelector(keyPath: \.playoutVolume))
                .receive(on: DispatchQueue.main)
                .sink { [weak cell] value in
                    guard let sliderCell = cell else { return }
                    sliderCell.valueLabel.text = "\(value)"
                    sliderCell.configSlider.value = Float(value)
                }
                .store(in: &cancellableSet)
        }
        secondSection.append(musicVolume)

        var pitchAdjustment = MusicSliderItem(title: .pitchSetting)
        pitchAdjustment.min = -1.0
        pitchAdjustment.max = 1.0
        pitchAdjustment.currentValue = manager.karaokeState.musicPitch
        pitchAdjustment.valueChanged = { [weak self] value in
            self?.manager?.setMusicPitch(pitch: value)
        }

        pitchAdjustment.subscribeState = { [weak self] cell, cancellableSet in
            guard let self = self else { return }
            self.manager?.subscribe(StateSelector(keyPath: \.musicPitch))
                .receive(on: DispatchQueue.main)
                .sink { [weak cell] value in
                    cell?.valueLabel.text = String(format: "%.1f", value)
                    cell?.configSlider.value = Float(value)
                }
                .store(in: &cancellableSet)
        }
        secondSection.append(pitchAdjustment)

        return secondSection
    }
}

fileprivate extension String {
    static var voiceEarMonitor: String = ("Ear Monitor").localized

    static let voiceEarMonitorVolume: String = ("Ear Monitor Volume").localized

    static var backgroundMusic: String = ("Music").localized

    static var chooseMusic: String = ("Choose Music").localized

    static var audioSetting: String = ("Audio settings").localized

    static var musicVolume: String = ("Music volume").localized

    static var microphoneVolume: String = ("Voice volume").localized

    static var captureVolume: String = ("Capture Volume").localized

    static var playoutVolume: String = ("Playout Volume").localized

    static var voicePitch: String = ("Music Pitch").localized

    static var changer: String = ("Voice changer").localized

    static var reverb: String = ("Reverb").localized

    static var Original: String = ("Original").localized

    static var child: String = ("Naughty child").localized

    static var girl: String = ("Loli").localized

    static var uncle: String = ("Uncle").localized

    static var ethereal: String = ("Ethereal").localized

    static var withoutEffect: String = ("None").localized

    static var karaoke: String = ("KTV").localized

    static var metal: String = ("Metallic sound").localized

    static var low: String = ("Low").localized

    static var loud: String = ("Loud").localized

    static var done: String = ("Done").localized

    static var score: String = ("Score").localized

    static var pitchSetting: String = ("Pitch Shift").localized
}
