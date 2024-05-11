//
//  MusicPanelView.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/4/28.
//

import UIKit
import Combine

class MusicPanelView: UIView {
    
    @Injected private var menuGenerator: MusicPanelMenuDataGenerator
    @WeakLazyInjected private var store: MusicPanelStoreProvider?
    
    var backButtonClickClosure: ((UIButton)->Void)?
    
    private var cancellableSet = Set<AnyCancellable>()
    private lazy var getIsPlaying = self.store?.select(MusicPanelSelectors.getIsPlaying)
    private lazy var getMusicInfoList = self.store?.select(MusicPanelSelectors.getMusicInfoList)
    
    private var isViewReady: Bool = false
    
    private let backButton: UIButton = {
        let view = UIButton(type: .system)
        view.setBackgroundImage(.liveBundleImage("live_back_icon"), for: .normal)
        return view
    }()
    
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
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        backgroundColor = .clear
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        subscribeMusicPanelState()
        bindInteraction()
        setupViewStyle()
        isViewReady = true
    }
    
    func constructViewHierarchy() {
        addSubview(backButton)
        addSubview(titleLabel)
        addSubview(tableView)
    }
    
    func activateConstraints() {
        backButton.snp.remakeConstraints { make in
            make.leading.equalToSuperview().inset(20)
            make.top.equalToSuperview().inset(20)
            make.height.equalTo(24.scale375())
            make.width.equalTo(24.scale375())
        }
        titleLabel.snp.remakeConstraints { make in
            make.centerY.equalTo(backButton)
            make.centerX.equalToSuperview()
            make.height.equalTo(24.scale375())
            make.width.equalTo(titleLabel.mm_w)
        }
        tableView.snp.remakeConstraints { make in
            make.bottom.equalToSuperview()
            make.trailing.equalToSuperview().offset(-16)
            make.leading.equalToSuperview().offset(16)
            make.top.equalTo(titleLabel.snp.bottom).offset(20)
            make.height.equalTo(height)        }
    }
    
    func bindInteraction() {
        tableView.dataSource = self
        backButton.addTarget(self, action: #selector(clickBack(sender:)), for: .touchUpInside)
    }
    
    func subscribeMusicPanelState() {
        getIsPlaying?
            .receive(on: RunLoop.main)
            .sink { [weak self] _ in
                guard let self = self else { return }
                self.tableView.reloadData()
            }
            .store(in: &cancellableSet)
        getMusicInfoList?
            .receive(on: RunLoop.main)
            .sink { [weak self] _ in
                guard let self = self else { return }
                self.tableView.reloadData()
            }
            .store(in: &cancellableSet)
    }
    
    func setupViewStyle() {
        backgroundColor = .g2
    }
    
    func resetMusicPanelState() {
        if let store = store, let currentPlayMusic = store.selectCurrent(MusicPanelSelectors.getCurrentPlayMusic) {
            store.dispatch(action: MusicPanelActions.stopPlayMusic(payload: currentPlayMusic))
        }
    }
    
    var height: CGFloat {
        return (self.window?.windowScene?.screen.bounds.height ?? 812)  * 0.65
    }
}

extension MusicPanelView: UITableViewDataSource {
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return menuGenerator.musicPanelMenus.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let item = menuGenerator.musicPanelMenus[indexPath.row]
        let cell = tableView.dequeueReusableCell(withIdentifier:MusicInfoItemCell.identifier, for: indexPath)
        if let musicInfoItemCell = cell as? MusicInfoItemCell {
            musicInfoItemCell.musicInfoItem = item
        }
        return cell
    }
}

extension MusicPanelView {
    @objc
    func clickBack(sender: UIButton) {
        self.backButtonClickClosure?(sender)
    }
}

extension String {
   fileprivate static let musicPanelTitle = localized("live.anchor.link.music.play.title")
}
