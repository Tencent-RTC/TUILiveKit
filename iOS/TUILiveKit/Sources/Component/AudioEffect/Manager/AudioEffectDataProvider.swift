//
//  AudioEffectDataProvider.swift
//  TUILiveKit
//
//  Created by aby on 2024/4/3.
//
import Combine
import RTCCommon
import AtomicXCore

protocol AudioEffectMenuDateGenerator {
    typealias Section = Int
    var audioEffectMenus: [Section: [SettingItem]] { get }
    var audioEffectSectionTitles: [Section: String] { get }
}

class AudioEffectDataProvider {
    
    private weak var manager: AudioEffectManager?
    init(manager: AudioEffectManager) {
        self.manager = manager
    }
    
    typealias AudioChangerDataSource = (title: String, icon: UIImage?, changerType: AudioChangerType)
    typealias AudioReverbDataSource = (title: String, icon: UIImage?, reverbType: AudioReverbType)
    
    let changerDataSource: [AudioChangerDataSource] = [
        (
            title: .noneVoiceText,
            icon: internalImage("live_audio_none"),
            changerType: .none
        ),
        (
            title: .childText,
            icon: internalImage("live_audio_changer_child"),
            changerType: .child
        ),
        (
            title: .girlText,
            icon: internalImage("live_audio_changer_girl"),
            changerType: .littleGirl
        ),
        (
            title: .uncleText,
            icon: internalImage("live_audio_changer_uncle"),
            changerType: .man
        ),
        (
            title: .etherealText,
            icon: internalImage("live_audio_changer_ethereal"),
            changerType: .ethereal
        ),
    ]
    
    let reverbDataSource: [AudioReverbDataSource] = [
        (
            title: .withoutEffectText,
            icon: internalImage("live_audio_none"),
            reverbType: .none
        ),
        (
            title: .karaokeText,
            icon: internalImage("live_audio_reverb_karaoke"),
            reverbType: .ktv
        ),
        (
            title: .metalText,
            icon: internalImage("live_audio_reverb_metal"),
            reverbType: .metallic
        ),
        (
            title: .lowText,
            icon: internalImage("live_audio_reverb_low"),
            reverbType: .deep
        ),
        (
            title: .loudText,
            icon: internalImage("live_audio_reverb_loud"),
            reverbType: .loud
        ),
    ]
}

extension AudioEffectDataProvider: AudioEffectMenuDateGenerator {
    
    var audioEffectMenus: [Section : [SettingItem]] {
        return generateAudioEffectData()
    }
    
    var audioEffectSectionTitles: [Section : String] {
        return [
            0: "",
            1: .audioSettingText,
            2: .changerText,
            3: .reverbText,
        ]
    }
    
    func generateAudioEffectData() -> [Section : [SettingItem]] {
        var menus: [Int:[SettingItem]] = [:]
        // first section settings.
        menus[0] = firstSectionMenus()
        // second section settings.
        menus[1] = secondSectionMenus()
        // third section settings
        menus[2] = thirdSectionMenus()
        // forth section setting.
        menus[3] = fourthSectionMenus()
        return menus
    }
    
    private func firstSectionMenus() -> [SettingItem] {
        guard let manager = manager else { return [] }
        var firstSection: [SettingItem] = []
        var earMonitor = SwitchItem(title: .voiceEarMonitorText, isOn: manager.audioState.isEarMonitorOpened)
        earMonitor.action = { [weak self] isOpened in
            guard let self = self else { return }
            self.manager?.setVoiceEarMonitorEnable(isOpened)
        }
        firstSection.append(earMonitor)
        var earMonitorVolume = SliderItem(title: .voiceEarMonitorVolumeText)
        earMonitorVolume.min = 0
        earMonitorVolume.max = 100
        earMonitorVolume.currentValue = Float(manager.audioState.earMonitorVolume)
        earMonitorVolume.valueDidChanged = { [weak self] value in
            guard let self = self else { return }
            self.manager?.setVoiceEarMonitorVolume(Int(value))
        }
        earMonitorVolume.subscribeState = { [weak self] cell, cancellables in
            guard let self = self else { return }
            self.manager?.subscribeState(StatePublisherSelector(keyPath: \AudioEffectState.earMonitorVolume))
                .receive(on: DispatchQueue.main)
                .sink { [weak cell] value in
                    guard let sliderCell = cell else { return }
                    sliderCell.configSlider.value = Float(value)
                    sliderCell.valueLabel.text = "\(value)"
                }
                .store(in: &cancellables)
        }
        firstSection.append(earMonitorVolume)
        return firstSection
    }
    
    private func secondSectionMenus() -> [SettingItem] {
        guard let manager = manager else { return [] }
        var secondSection:[SettingItem] = []
        var microphoneVolume = SliderItem(title: .microphoneVolumeText)
        microphoneVolume.min = 0
        microphoneVolume.max = 100
        microphoneVolume.currentValue = Float(manager.deviceState.outputVolume)
        microphoneVolume.valueDidChanged = { [weak self] value in
            guard let self = self else { return }
            self.manager?.setMicrophoneVolume(Int(value))
        }
        microphoneVolume.subscribeState = { [weak self] cell, cancellableSet in
            guard let self = self else { return }
            self.manager?.subscribeState(StatePublisherSelector(keyPath: \DeviceState.outputVolume))
                .receive(on: DispatchQueue.main)
                .sink { [weak cell] value in
                    guard let sliderCell = cell else { return }
                    sliderCell.configSlider.value = Float(value)
                    sliderCell.valueLabel.text = "\(value)"
                }
                .store(in: &cancellableSet)
        }
        secondSection.append(microphoneVolume)
        return secondSection
    }
    
    private func thirdSectionMenus() -> [SettingItem] {
        return self.changerDataSource.map { (title: String, icon: UIImage?, changerType: AudioChangerType) in
            return self.createAudioChangerMenu(title: title, icon: icon, changerType: changerType)
        }
    }
    
    private func createAudioChangerMenu(title: String, icon: UIImage?, changerType: AudioChangerType) -> ButtonItem {
        var item = ButtonItem(buttonTitle: title)
        guard let manager = manager else { return item }
        item.icon = icon
        item.isSelected = manager.audioState.audioChangerType == changerType
        item.action = { [weak self] in
            guard let self = self else { return }
            self.manager?.setChangerType(changerType)
        }
        return item
    }
    
    private func fourthSectionMenus() -> [SettingItem] {
        return self.reverbDataSource.map { (title: String, icon: UIImage?, reverbType: AudioReverbType) in
            return self.createAudioReverbMenu(title: title, icon: icon, reverbType: reverbType)
        }
    }
    
    private func createAudioReverbMenu(title: String, icon: UIImage?, reverbType: AudioReverbType) -> ButtonItem {
        var item = ButtonItem(buttonTitle: title)
        guard let manager = manager else { return item }
        item.icon = icon
        item.isSelected = manager.audioState.audioReverbType == reverbType
        item.action = { [weak self] in
            guard let self = self else { return }
            self.manager?.setReverbType(reverbType)
        }
        return item
    }
}
