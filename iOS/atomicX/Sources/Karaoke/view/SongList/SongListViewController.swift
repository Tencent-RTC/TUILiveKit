//
//  SongListViewController.swift
//  Pods
//
//  Created by ssc on 2025/8/7.
//

import UIKit
import RTCCommon
import SnapKit
import Combine
import RTCRoomEngine

public class SongListViewController: UIViewController {
    enum ListType {
        case allSongs
        case selectedSongs
    }

    private var currentListType: ListType = .allSongs {
        didSet {
            UIView.transition(with: tableView, duration: 0.3, options: .transitionCrossDissolve, animations: {
                self.tableView.reloadData()
            })
        }
    }

    private let contentView = UIView()
    private let isOwner: Bool
    private let isKTV: Bool
    private weak var karaokeManager: KaraokeManager?
    private weak var popupViewController: UIViewController?
    private var songs: [MusicInfo] = []

    private let tabContainer: UIView = {
        let view = UIView()
        view.backgroundColor = UIColor("1F2024")
        return view
    }()

    private let songListButton: UIButton = {
        let button = UIButton()
        button.setTitle(.orderedText, for: .normal)
        button.titleLabel?.font = UIFont.systemFont(ofSize: 16, weight: .medium)
        button.setTitleColor(UIColor(white: 1, alpha: 0.9), for: .normal)
        return button
    }()

    private let selectedListButton: UIButton = {
        let button = UIButton()
        button.setTitle(.orderedText, for: .normal)
        button.titleLabel?.font = UIFont.systemFont(ofSize: 16, weight: .regular)
        button.setTitleColor(UIColor(white: 1, alpha: 0.3), for: .normal)
        return button
    }()

    private let backButton: UIButton = {
        let button = UIButton()
        button.setImage(UIImage.atomicXBundleImage(named: "ktv_exit"), for: .normal)
        button.titleLabel?.font = UIFont.systemFont(ofSize: 16, weight: .regular)
        button.setTitleColor(UIColor(white: 1, alpha: 0.3), for: .normal)
        return button
    }()

    private lazy var backView : UIView = {
        let view = UIView()
        view.isHidden = !isOwner
        return view
    }()

    private let backLabel: UILabel = {
        let label = UILabel()
        label.text = .exitOrder
        label.textColor = UIColor.redColor
        label.font = UIFont(name: "PingFangSC-Semibold", size: 14) ?? .systemFont(ofSize: 14, weight: .regular)
        return label
    }()

    private lazy var tableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .plain)
        tableView.register(SongListCell.self, forCellReuseIdentifier: "SongListCell")
        tableView.register(SelectedSongCell.self, forCellReuseIdentifier: "SelectedSongCell")
        tableView.delegate = self
        tableView.dataSource = self
        tableView.rowHeight = 64
        tableView.separatorStyle = .none
        tableView.backgroundColor = UIColor("1F2024")
        return tableView
    }()

    public init(karaokeManager: KaraokeManager, isOwner: Bool, isKTV: Bool) {
        self.karaokeManager = karaokeManager
        self.isOwner = isOwner
        self.isKTV = isKTV
        super.init(nibName: nil, bundle: nil)
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        setupViewStyle()
        setupStateSubscriptions()
        karaokeManager.show()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var cancellables = Set<AnyCancellable>()
    private var isViewReady = false
    
    private func setupStateSubscriptions() {
        guard let karaokeManager = karaokeManager else {return}
        karaokeManager.subscribe(StateSelector(keyPath: \.songLibrary))
            .receive(on: DispatchQueue.main)
            .removeDuplicates()
            .sink { [weak self] songs in
                guard let self = self else {return}
                self.songs = songs
                self.tableView.reloadData()
            }
            .store(in: &cancellables)

        karaokeManager.subscribe(StateSelector(keyPath: \.selectedSongs))
            .removeDuplicates()
            .receive(on: DispatchQueue.main)
            .sink { [weak self] selectedSongs in
                guard let self = self else {return}
                self.updateSelectedCount()
                self.tableView.reloadData()
            }
            .store(in: &cancellables)

        karaokeManager.subscribe(StateSelector(keyPath: \.currentMusicId))
            .removeDuplicates()
            .dropFirst()
            .receive(on: DispatchQueue.main)
            .sink { [weak self] _ in
                guard let self = self else {return}
                self.tableView.reloadData()
            }
            .store(in: &cancellables)

        karaokeManager.kickedOutSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] in
                guard let self = self else { return }
                guard let vc = WindowUtils.getCurrentWindowViewController() else {return }
                vc.dismiss(animated: true)
            }
            .store(in: &cancellables)
    }

    private func constructViewHierarchy() {
        contentView.backgroundColor = UIColor("1F2024")
        view.addSubview(contentView)
        contentView.addSubview(tabContainer)
        tabContainer.addSubview(songListButton)
        tabContainer.addSubview(selectedListButton)
        tabContainer.addSubview(backView)
        backView.addSubview(backLabel)
        backView.addSubview(backButton)
        contentView.addSubview(tableView)
    }

    private func activateConstraints() {
        contentView.snp.makeConstraints { make in
            make.leading.trailing.bottom.equalToSuperview()
            make.height.equalTo(440.scale375())
            make.width.equalTo(375.scale375())
            make.bottom.equalToSuperview()
        }

        if !isKTV {
            backView.snp.makeConstraints { make in
                make.right.equalToSuperview()
                make.centerY.equalToSuperview()
                make.height.equalTo(28.scale375())
            }

            backButton.snp.makeConstraints { make in
                make.left.equalToSuperview()
                make.centerY.equalToSuperview()
                make.height.width.equalTo(16.scale375())
            }

            backLabel.snp.makeConstraints { make in
                make.left.equalTo(backButton.snp.right).offset(6.scale375())
                make.centerY.equalToSuperview()
                make.right.equalToSuperview().offset(-20.scale375())
            }
        }

        tabContainer.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(10.scale375())
            make.leading.trailing.equalToSuperview()
            make.height.equalTo(28.scale375())
        }

        songListButton.snp.makeConstraints { make in
            make.left.equalToSuperview().offset(16.scale375())
            make.centerY.equalToSuperview()
            make.height.equalTo(24.scale375())
        }

        selectedListButton.snp.makeConstraints { make in
            make.left.equalTo(songListButton.snp.right).offset(20.scale375())
            make.centerY.equalToSuperview()
            make.height.equalTo(24.scale375())
        }

        tableView.snp.makeConstraints { make in
            make.top.equalTo(tabContainer.snp.bottom).offset(10.scale375())
            make.leading.trailing.bottom.equalToSuperview()
        }
    }

    private func bindInteraction() {
        selectedListButton.addTarget(self, action: #selector(onSelectedListButtonTapped), for: .touchUpInside)
        songListButton.addTarget(self, action: #selector(onSongListButtonTapped), for: .touchUpInside)
        backView.addTapGesture(target: self, action: #selector(onBackViewTapped))

        let tap = UITapGestureRecognizer(target: self, action: #selector(onBackButtonClickedClosure(_:)))
        self.view.addGestureRecognizer(tap)
    }

    @objc private func onSongListButtonTapped() {
        currentListType = .allSongs
        updateTabAppearance()
    }

    @objc private func onSelectedListButtonTapped() {
        currentListType = .selectedSongs
        updateTabAppearance()
    }

    @objc private func onBackViewTapped() {
        guard let karaokeManager = karaokeManager , let vc = WindowUtils.getCurrentWindowViewController() else {return }
        karaokeManager.exit()
        vc.dismiss(animated: true)
    }

    @objc private func onBackButtonClickedClosure(_ sender: UITapGestureRecognizer) {
        let point = sender.location(in: view)
        if !contentView.frame.contains(point) {
            guard let vc = WindowUtils.getCurrentWindowViewController() else {return }
            vc.dismiss(animated: true)
        }
    }

    private func switchToList(type: ListType) {
        UIView.transition(with: self.view, duration: 0.3, options: .transitionCrossDissolve, animations: {
            self.currentListType = type
            self.tableView.reloadData()
        })
    }

    private func updateTabAppearance() {
        let isSongList = currentListType == .allSongs
        songListButton.setTitleColor(UIColor(white: 1, alpha: isSongList ? 0.9 : 0.3), for: .normal)
        songListButton.titleLabel?.font = UIFont.systemFont(ofSize: 16, weight: isSongList ? .medium : .regular)

        selectedListButton.setTitleColor(UIColor(white: 1, alpha: isSongList ? 0.3 : 0.9), for: .normal)
        selectedListButton.titleLabel?.font = UIFont.systemFont(ofSize: 16, weight: isSongList ? .regular : .medium)

        UIView.animate(withDuration: 0.2) {
            self.view.layoutIfNeeded()
        }
    }

    private func setupViewStyle() {
        view.backgroundColor = .clear
        tableView.backgroundColor = UIColor("1F2024")
        tableView.separatorStyle = .none
    }
    
    deinit {
        cancellables.forEach { $0.cancel() }
        cancellables.removeAll()
        tableView.delegate = nil
        tableView.dataSource = nil
        songListButton.removeTarget(nil, action: nil, for: .allEvents)
        selectedListButton.removeTarget(nil, action: nil, for: .allEvents)
        karaokeManager = nil
    }
}


extension SongListViewController: UITableViewDataSource, UITableViewDelegate{
    public func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        guard let karaokeManager = karaokeManager else { return 0 }
        return currentListType == .allSongs ? songs.count : karaokeManager.karaokeState.selectedSongs.count
    }

    public func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        guard let karaokeManager = karaokeManager else { return UITableViewCell()}
        if currentListType == .allSongs {
            let cell = tableView.dequeueReusableCell(withIdentifier: "SongListCell", for: indexPath) as! SongListCell
            cell.karaokeManager = karaokeManager
            cell.isOwner = isOwner
            let isSelected = karaokeManager.karaokeState.isSongSelected(songs[indexPath.row].musicId)
            cell.configure(with: songs[indexPath.row], indexPath: indexPath, isSelected: isSelected)
            return cell
        } else {
            let cell = tableView.dequeueReusableCell(withIdentifier: "SelectedSongCell", for: indexPath) as! SelectedSongCell
            cell.karaokeManager = karaokeManager
            cell.isOwner = isOwner
            let musicId = karaokeManager.karaokeState.selectedSongs[indexPath.row].musicId
            if let song = songs.first(where: { $0.musicId == musicId }) {
                cell.configure(with: song, at: indexPath.row)
            }
            return cell
        }
    }


    private func updateSelectedCount() {
        guard let karaokeManager = karaokeManager else {return}
        selectedListButton.setTitle(.orderedCountText.localized(replace: "\(karaokeManager.karaokeState.selectedSongs.count)"),for: .normal)
    }

}

fileprivate extension String {
    static var orderedText: String = ("Ordered").localized
    static var orderedCountText: String = ("Ordered(xxx)").localized
    static var exitOrder: String = ("Exit Order").localized
}
