//
//  File.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/20.
//

import Foundation
#if TXLiteAVSDK_TRTC
    import TXLiteAVSDK_TRTC
#elseif TXLiteAVSDK_Professional
    import TXLiteAVSDK_Professional
#endif

enum MusicPlayAction {
    case `default`
    case startPlay(musicInfo: MusicInfo)
    case pausePlay(musicInfo: MusicInfo)
    case stopPlay(musicInfo: MusicInfo)
    case delete(musicInfo: MusicInfo)
    case add(musicInfo: MusicInfo)
}

class AnchorMusicPlayPanel: UIView {
    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()

    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
    }

    private var popupAction: Observable<PopupPanelAction>?
    private var liveRoomInfo:LiveRoomInfo {
        engineService.liveRoomInfo
    }
    private let engineService: RoomEngineService
    init(engineService: RoomEngineService) {
        self.engineService = engineService
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.contentMode = .center
        label.font = .customFont(ofSize: 16, weight: .medium)
        label.textColor = .g7
        label.text = .musicPlayTitle
        return label
    }()

    private lazy var musicListTableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .plain)
        tableView.backgroundColor = .clear
        tableView.separatorStyle = .none
        tableView.delegate = self
        tableView.dataSource = self
        tableView.register(MusicInfoCell.self, forCellReuseIdentifier: MusicInfoCell.cellReuseIdentifier)
        tableView.register(MusicInfoPlayCell.self, forCellReuseIdentifier: MusicInfoPlayCell.cellReuseIdentifier)
        engineService.liveKitStore.musicList.addObserver(self) { [weak self] _, _ in
            self?.updateView()
        }
        return tableView
    }()

    private func updateView() {
        musicListTableView.reloadData()
    }
}

// MARK: Layout

extension AnchorMusicPlayPanel {
    func constructViewHierarchy() {
        backgroundColor = .g2
        layer.cornerRadius = 16
        layer.masksToBounds = true
        addSubview(titleLabel)
        addSubview(musicListTableView)
    }

    func activateConstraints() {
        snp.remakeConstraints { make in
            if isPortrait {
                make.width.equalToSuperview()
                make.height.equalTo(352.scale375Height())
                make.centerX.equalToSuperview()
            } else {
                make.width.equalTo(375)
                make.height.equalToSuperview()
                make.trailing.equalToSuperview()
            }
            make.bottom.equalToSuperview()
        }

        titleLabel.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(20.scale375Height())
            make.centerX.equalToSuperview()
            make.height.equalTo(24.scale375Height())
        }

        musicListTableView.snp.remakeConstraints { make in
            make.leading.trailing.bottom.equalToSuperview()
            make.top.equalTo(titleLabel.snp.bottom).offset(20)
        }
    }
}

// MARK: Action

extension AnchorMusicPlayPanel {
    private func musicAction(action: MusicPlayAction) {
        switch action {
        case let .startPlay(musicInfo):
            playMusic(musicInfo: musicInfo)
            return
        case let .stopPlay(musicInfo):
            stopMusic(musicInfo: musicInfo)
            return
        case let .delete(musicInfo: musicInfo):
            delete(musicInfo: musicInfo)
            return
        default:
            return
        }
    }

    private func stopMusic(musicInfo: MusicInfo) {
        musicInfo.isPlaying.value = false
        engineService.stopPlayMusic(musicInfo.id)
    }

    private func playMusic(musicInfo: MusicInfo) {
        if let playingMusicInfo = engineService.liveKitStore.musicList.value.first(where: { $0.isPlaying.value }) {
            stopMusic(musicInfo: playingMusicInfo)
        }

        musicInfo.isPlaying.value = true
        engineService.liveKitStore.currentMusicInfo.value = musicInfo
        engineService.startPlayMusic(musicInfo)
    }

    private func delete(musicInfo: MusicInfo) {
        UIAlertController.showAlertController(title: .alertTipsTitle,
                                              message: .localizedReplace(.alertDeleteDesc, replace: musicInfo.name),
                                              cancel: .alertCancel,
                                              sure: .alertSure) { [weak self] sure in
            guard let self = self else { return }
            if sure {
                engineService.liveKitStore.musicList.value = engineService.liveKitStore.musicList.value.filter({ $0.id != musicInfo.id })
                if musicInfo.isPlaying.value {
                    self.stopMusic(musicInfo: musicInfo)
                    engineService.liveKitStore.currentMusicInfo.value = MusicInfo()
                }
            }
        }
    }
}

extension AnchorMusicPlayPanel: UITableViewDataSource {
    internal func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return engineService.liveKitStore.musicList.value.count
    }

    func numberOfSections(in tableView: UITableView) -> Int {
        return 1
    }
}

extension AnchorMusicPlayPanel: UITableViewDelegate {
    internal func tableView(_ tableView: UITableView,
                            cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        var cell: MusicInfoCell?
        if indexPath.row < engineService.liveKitStore.musicList.value.count {
            cell = tableView.dequeueReusableCell(withIdentifier: MusicInfoPlayCell.cellReuseIdentifier, for: indexPath) as? MusicInfoPlayCell
            cell?.musicInfo = engineService.liveKitStore.musicList.value[indexPath.row]
            cell?.action.addObserver(self, closure: { [weak self] action, _ in
                self?.musicAction(action: action)
            })
        }
        return cell ?? tableView.dequeueReusableCell(withIdentifier: MusicInfoCell.cellReuseIdentifier, for: indexPath)
    }

    internal func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    }

    internal func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return 50.scale375()
    }
}

extension AnchorMusicPlayPanel: PopupPanelSubViewProtocol {
    func setAction(_ popupAction: Observable<PopupPanelAction>) {
        self.popupAction = popupAction
    }

    func updateRootViewOrientation(isPortrait: Bool) {
        self.isPortrait = isPortrait
        activateConstraints()
    }
}

private extension String {
    static var musicPlayTitle: String {
        localized("live.anchor.link.music.play.title")
    }

    static var alertTipsTitle: String {
        localized("live.alert.tips.title")
    }

    static var alertDeleteDesc: String {
        localized("live.alert.delete.desc.xxx")
    }

    static var alertCancel: String {
        localized("live.alert.cancel")
    }

    static var alertSure: String {
        localized("live.alert.sure")
    }
}
