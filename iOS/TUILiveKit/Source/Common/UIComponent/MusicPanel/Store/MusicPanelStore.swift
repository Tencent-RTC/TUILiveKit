//
//  MusicPanelStoreProvider.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/4/28.
//
import Combine

#if canImport(TXLiteAVSDK_TRTC)
    import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
    import TXLiteAVSDK_Professional
#endif

class MusicPanelStore {
    
    let service = MusicPanelService()
    private(set) lazy var store: Store<MusicPanelState, MusicPanelService> = Store(initialState: MusicPanelState(), environment: self.service)
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    init() {
        initializeStore()
    }
    
    private func initializeStore() {
        store.register(reducer: musicPanelReducer)
        store.register(effects: MusicPanelEffects())
    }
}

extension MusicPanelStore: MusicPanelStoreProvider {
    func dispatch(action: Action) {
        store.dispatch(action: action)
    }
    
    func select<Value>(_ selector: Selector<MusicPanelState, Value>) -> AnyPublisher<Value, Never> where Value : Equatable {
       return store.select(selector)
            .removeDuplicates()
            .eraseToAnyPublisher()
    }
    
    func selectCurrent<Value>(_ selector: Selector<MusicPanelState, Value>) -> Value {
       return store.selectCurrent(selector)
    }
}

extension MusicPanelStore: MusicPanelMenuDataGenerator {
    var musicPanelMenus: [MusicInfoCellItem] {
        return generateMusicPanelData()
    }
    
    func generateMusicPanelData() -> [MusicInfoCellItem] {
        var musicInfoArray: [MusicInfoCellItem] = []
        for musicInfo in store.selectCurrent(MusicPanelSelectors.getMusicInfoList) {
            var musicInfoCellItem = MusicInfoCellItem(title: musicInfo.name, musicInfo: musicInfo)
            musicInfoCellItem.startPlay = { [weak self] musicInfo in
                guard let self = self else { return }
                if let currentPlayMusic = self.store.selectCurrent(MusicPanelSelectors.getCurrentPlayMusic) {
                    self.store.dispatch(action: MusicPanelActions.stopPlayMusic(payload: currentPlayMusic))
                }
                self.store.dispatch(action: MusicPanelActions.startPlayMusic(payload: musicInfo))
            }
            
            musicInfoCellItem.stopPlay = { [weak self] musicInfo in
                guard let self = self else { return }
                self.store.dispatch(action: MusicPanelActions.stopPlayMusic(payload: musicInfo))
            }
            
            musicInfoCellItem.deleteMusic = { [weak self] musicInfo in
                guard let self = self else { return }
                if let currentPlayMusic = self.store.selectCurrent(MusicPanelSelectors.getCurrentPlayMusic) {
                    self.store.dispatch(action: MusicPanelActions.stopPlayMusic(payload: currentPlayMusic))
                }
                self.store.dispatch(action: MusicPanelActions.deleteMusic(payload: musicInfo))
            }
            
            musicInfoArray.append(musicInfoCellItem)
        }
        return musicInfoArray
    }
}
