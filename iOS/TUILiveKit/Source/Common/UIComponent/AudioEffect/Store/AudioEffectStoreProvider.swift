//
//  AudioEffectStoreImpl.swift
//  TUILiveKit
//
//  Created by aby on 2024/4/3.
//
import Combine

class AudioEffectStore {
    typealias AudioChangerDataSource = (title: String, icon: UIImage?, changerType: AudioChangerType)
    typealias AudioReverbDataSource = (title: String, icon: UIImage?, reverbType: AudioReverbType)
    
    let service = AudioEffectService()
    private(set) lazy var store: Store<AudioEffectState, AudioEffectService> = Store(initialState: AudioEffectState(), environment: self.service)
    
    
    init() {
        initializeStore()
    }
    
    private func initializeStore() {
        store.register(reducer: audioEffectReducer)
        store.register(effects: AudioEffectEffects())
    }
    
    let changerDataSource: [AudioChangerDataSource] = [
        (
            title: .originVoiceText,
            icon: .liveBundleImage("live_audio_none"),
            changerType: .none
        ),
        (
            title: .childText,
            icon: .liveBundleImage("live_audio_changer_child"),
            changerType: .child
        ),
        (
            title: .girlText,
            icon: .liveBundleImage("live_audio_changer_girl"),
            changerType: .littleGirl
        ),
        (
            title: .uncleText,
            icon: .liveBundleImage("live_audio_changer_uncle"),
            changerType: .man
        ),
        (
            title: .etherealText,
            icon: .liveBundleImage("live_audio_changer_ethereal"),
            changerType: .ethereal
        ),
    ]
    
    let reverbDataSource: [AudioReverbDataSource] = [
        (
            title: .withoutEffectText,
            icon: .liveBundleImage("live_audio_none"),
            reverbType: .none
        ),
        (
            title: .karaokeText,
            icon: .liveBundleImage("live_audio_reverb_karaoke"),
            reverbType: .KTV
        ),
        (
            title: .metalText,
            icon: .liveBundleImage("live_audio_reverb_metal"),
            reverbType: .metallic
        ),
        (
            title: .lowText,
            icon: .liveBundleImage("live_audio_reverb_low"),
            reverbType: .deep
        ),
        (
            title: .loudText,
            icon: .liveBundleImage("live_audio_reverb_loud"),
            reverbType: .loud
        ),
    ]
}

extension AudioEffectStore: AudioEffectStoreProvider {
    func dispatch(action: Action) {
        store.dispatch(action: action)
    }
    
    func select<Value>(_ selector: Selector<AudioEffectState, Value>) -> AnyPublisher<Value, Never> where Value : Equatable {
       return store.select(selector)
            .removeDuplicates()
            .eraseToAnyPublisher()
    }
    
    func selectCurrent<Value>(_ selector: Selector<AudioEffectState, Value>) -> Value {
       return store.selectCurrent(selector)
    }
}

extension AudioEffectStore: AudioEffectMenuDateGenerator {
    
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
        var firstSection: [SettingItem] = []
        var earMonitor = SwitchItem(title: .voiceEarMonitorText, isOn: store.selectCurrent(AudioEffectSelectors.isEarMonitorOpened))
        earMonitor.isOn = store.selectCurrent(AudioEffectSelectors.isEarMonitorOpened)
        earMonitor.action = { [weak self] isOpened in
            guard let self = self else { return }
            self.store.dispatch(action: AudioEffectActions.operateEarMonitor(payload: isOpened))
        }
        firstSection.append(earMonitor)
        var earMonitorVolume = SliderItem(title: .voiceEarMonitorVolumeText)
        earMonitorVolume.min = 0
        earMonitorVolume.max = 150
        earMonitorVolume.currentValue = Float(store.selectCurrent(AudioEffectSelectors.earMonitorVolume))
        earMonitorVolume.valueDidChanged = { [weak self] value in
            guard let self = self else { return }
            self.store.dispatch(action: AudioEffectActions.updateEarMonitorVolume(payload: Int(value)))
        }
        earMonitorVolume.subscribeState = { [weak self] cell, cancellables in
            guard let self = self else { return }
            self.store.select(AudioEffectSelectors.earMonitorVolume)
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
        var secondSection:[SettingItem] = []
        var musicVolume = SliderItem(title: .musicVolumeText)
        musicVolume.min = 0
        musicVolume.max = 150
        musicVolume.currentValue = Float(store.selectCurrent(AudioEffectSelectors.musicVolume))
        musicVolume.valueDidChanged = { [weak self] value in
            guard let self = self else { return }
            self.store.dispatch(action: AudioEffectActions.updateMusicVolume(payload: Int(value)))
        }
        musicVolume.subscribeState = { [weak self] cell, cancellables in
            guard let self = self else { return }
            self.store.select(AudioEffectSelectors.musicVolume)
                .receive(on: DispatchQueue.main)
                .sink { [weak cell] value in
                    guard let sliderCell = cell else { return }
                    sliderCell.configSlider.value = Float(value)
                    sliderCell.valueLabel.text = "\(value)"
                }
                .store(in: &cancellables)
        }
        secondSection.append(musicVolume)
        var microphoneVolume = SliderItem(title: .microphoneVolumeText)
        microphoneVolume.min = 0
        microphoneVolume.max = 150
        microphoneVolume.currentValue = Float(store.selectCurrent(AudioEffectSelectors.microphoneVolume))
        microphoneVolume.valueDidChanged = { [weak self] value in
            guard let self = self else { return }
            self.store.dispatch(action: AudioEffectActions.updateMicrophoneVolume(payload: Int(value)))
        }
        microphoneVolume.subscribeState = { [weak self] cell, cancellables in
            guard let self = self else { return }
            self.store.select(AudioEffectSelectors.microphoneVolume)
                .receive(on: DispatchQueue.main)
                .sink { [weak cell] value in
                    guard let sliderCell = cell else { return }
                    sliderCell.configSlider.value = Float(value)
                    sliderCell.valueLabel.text = "\(value)"
                }
                .store(in: &cancellables)
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
        item.icon = icon
        item.isSelected = store.selectCurrent(AudioEffectSelectors.changerType) == changerType
        item.action = { [weak self] in
            guard let self = self else { return }
            self.store.dispatch(action: AudioEffectActions.changerVoice(payload: changerType))
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
        item.icon = icon
        item.isSelected = store.selectCurrent(AudioEffectSelectors.reverbType) == reverbType
        item.action = { [weak self] in
            guard let self = self else { return }
            self.store.dispatch(action: AudioEffectActions.reverbVoice(payload: reverbType))
        }
        return item
    }
}
