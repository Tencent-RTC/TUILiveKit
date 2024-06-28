//
//  MusicPanelView.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/4/28.
//

import UIKit
import Combine
import RTCCommon
import RTCRoomEngine

class MusicPanelView: UIView {

    private let store: MusicPanelStoreProvider
    
    private var cancellableSet = Set<AnyCancellable>()
    private lazy var getIsPlaying = self.store.select(MusicPanelSelectors.getIsPlaying)
    private lazy var getMusicInfoList = self.store.select(MusicPanelSelectors.getMusicInfoList)
    
    private let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.contentMode = .center
        label.font = .customFont(ofSize: 16, weight: .medium)
        label.textColor = .g7
        label.text = .musicPanelTitle
        label.sizeToFit()
        return label
    }()
    
    private let tableView: UITableView = {
        let view = UITableView(frame: .zero, style: .grouped)
        view.register(MusicInfoItemCell.self, forCellReuseIdentifier: MusicInfoItemCell.identifier)
        view.separatorStyle = .none
        view.backgroundColor = .clear
        view.sectionFooterHeight = 0
        view.sectionHeaderHeight = 0
        view.rowHeight = 56
        view.showsVerticalScrollIndicator = false
        return view
    }()
    
    init(roomEngine: TUIRoomEngine? = nil) {
        self.store = MusicPanelStoreProvider(roomEngine: roomEngine)
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
        subscribeMusicPanelState()
        bindInteraction()
        setupViewStyle()
        isViewReady = true
    }
    
    func constructViewHierarchy() {
        addSubview(titleLabel)
        addSubview(tableView)
    }
    
    func activateConstraints() {
        titleLabel.snp.remakeConstraints { make in
            make.top.equalToSuperview().inset(20.scale375Height())
            make.centerX.equalToSuperview()
            make.height.equalTo(24.scale375())
            make.width.equalTo(titleLabel.mm_w)
        }
        tableView.snp.remakeConstraints { make in
            make.bottom.equalToSuperview()
            make.trailing.equalToSuperview().offset(-16.scale375())
            make.leading.equalToSuperview().offset(16.scale375())
            make.top.equalTo(titleLabel.snp.bottom).offset(20.scale375Height())
            make.height.equalTo(height)
        }
    }
    
    func bindInteraction() {
        tableView.dataSource = self
    }
    
    func subscribeMusicPanelState() {
        getIsPlaying
            .receive(on: RunLoop.main)
            .sink { [weak self] _ in
                guard let self = self else { return }
                self.tableView.reloadData()
            }
            .store(in: &cancellableSet)
        getMusicInfoList
            .receive(on: RunLoop.main)
            .sink { [weak self] _ in
                guard let self = self else { return }
                self.tableView.reloadData()
            }
            .store(in: &cancellableSet)
    }
    
    func setupViewStyle() {
        backgroundColor = .g2
        layer.cornerRadius = 20
        layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
    }
    
    func resetMusicPanelState() {
        if let currentPlayMusic = store.selectCurrent(MusicPanelSelectors.getCurrentPlayMusic) {
            store.dispatch(action: MusicPanelActions.stopPlayMusic(payload: currentPlayMusic))
        }
    }
    
    var height: CGFloat {
        return (self.window?.windowScene?.screen.bounds.height ?? 812)  * 0.35
    }
}

extension MusicPanelView: UITableViewDataSource {
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return store.musicPanelMenus.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let item = store.musicPanelMenus[indexPath.row]
        let cell = tableView.dequeueReusableCell(withIdentifier:MusicInfoItemCell.identifier, for: indexPath)
        if let musicInfoItemCell = cell as? MusicInfoItemCell {
            musicInfoItemCell.musicInfoItem = item
        }
        return cell
    }
}

extension String {
   fileprivate static let musicPanelTitle = localized("live.anchor.link.music.play.title")
}
