//
//  MusicInfoItemCell.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/4/28.
//

import Foundation
import TUICore
import UIKit
import Combine

class MusicInfoItemCell: UITableViewCell {
    static let identifier = "MusicInfoItemCell"
    private var cancellableSet: Set<AnyCancellable> = []
    
    var musicInfoItem: MusicInfoCellItem? {
        didSet {
            guard let musicInfo = musicInfoItem?.musicInfo else {
                return
            }
            nameLabel.text = musicInfo.name
            updateView()
        }
    }
    
    let nameLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 16)
        if TUIGlobalization.getRTLOption() {
            label.textAlignment = .right
        } else {
            label.textAlignment = .left
        }
        label.textColor = .g7
        return label
    }()
    
    let lineView: UIView = {
        let view = UIView()
        view.backgroundColor = .g3.withAlphaComponent(0.3)
        return view
    }()
    
    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        selectionStyle = .none
        backgroundColor = .clear
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    lazy var startPlayButton: UIButton = {
        let view = UIButton(type: .system)
        view.showsTouchWhenHighlighted = false
        view.setBackgroundImage(.liveBundleImage("live_music_start_play"), for: .normal)
        view.sizeToFit()
        view.addTarget(self, action: #selector(startPlayButtonClick), for: .touchUpInside)
        view.isHidden = true
        return view
    }()

    lazy var stopPlayButton: UIButton = {
        let view = UIButton(type: .system)
        view.showsTouchWhenHighlighted = false
        view.setBackgroundImage(.liveBundleImage("live_music_pause_play"), for: .normal)
        view.sizeToFit()
        view.addTarget(self, action: #selector(stopPlayButtonClick), for: .touchUpInside)
        view.isHidden = true
        return view
    }()

    private lazy var deleteButton: UIButton = {
        let view = UIButton(type: .system)
        view.showsTouchWhenHighlighted = false
        view.setBackgroundImage(.liveBundleImage("live_music_delete"), for: .normal)
        view.sizeToFit()
        view.addTarget(self, action: #selector(deleteButtonClick), for: .touchUpInside)
        return view
    }()

    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else {
            return
        }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
    }
    
    override func prepareForReuse() {
        super.prepareForReuse()
        self.musicInfoItem = nil
        cancellableSet.removeAll()
    }

    func constructViewHierarchy() {
        contentView.addSubview(nameLabel)
        contentView.addSubview(startPlayButton)
        contentView.addSubview(stopPlayButton)
        contentView.addSubview(deleteButton)
        contentView.addSubview(lineView)
    }

    func activateConstraints() {
        nameLabel.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.height.equalToSuperview()
            make.leading.equalToSuperview().inset(5.scale375())
            make.trailing.equalToSuperview().inset(100.scale375())
        }

        deleteButton.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.trailing.equalToSuperview().inset(5.scale375())
            make.width.equalTo(deleteButton.mm_w)
            make.height.equalTo(deleteButton.mm_h)
        }

        startPlayButton.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.trailing.equalTo(deleteButton.snp.leading).offset(-16.scale375())
            make.width.equalTo(startPlayButton.mm_w)
            make.height.equalTo(startPlayButton.mm_w)
        }

        stopPlayButton.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.trailing.equalTo(deleteButton.snp.leading).offset(-16.scale375())
            make.width.equalTo(stopPlayButton.mm_w)
            make.height.equalTo(stopPlayButton.mm_w)
        }

        lineView.snp.makeConstraints { make in
            make.bottom.equalToSuperview()
            make.leading.equalTo(nameLabel)
            make.trailing.equalTo(deleteButton)
            make.height.equalTo(1)
        }
    }

    func updateView() {
        guard let musicInfoItem = musicInfoItem else { return }
        stopPlayButton.isHidden = !musicInfoItem.musicInfo.isPlaying
        startPlayButton.isHidden = musicInfoItem.musicInfo.isPlaying
    }
}

// MARK: Action

extension MusicInfoItemCell {
    @objc func startPlayButtonClick() {
        guard let musicInfoItem = musicInfoItem else { return }
        musicInfoItem.startPlay?(musicInfoItem.musicInfo)
    }

    @objc func stopPlayButtonClick() {
        guard let musicInfoItem = musicInfoItem else { return }
        musicInfoItem.stopPlay?(musicInfoItem.musicInfo)
    }

    @objc func deleteButtonClick() {
        guard let musicInfoItem = musicInfoItem else { return }
        UIAlertController.showAlertController(title: .alertTipsTitle,
                                              message: .localizedReplace(.alertDeleteDesc, replace: musicInfoItem.musicInfo.name),
                                              cancel: .alertCancel,
                                              sure: .alertSure) { [weak self] sure in
            guard let self = self else { return }
            if sure {
                musicInfoItem.deleteMusic?(musicInfoItem.musicInfo)
            }
        }
    }
}

private extension String {
    static var anchorHangUpTitle: String {
        localized("live.anchor.link.hang.up.title")
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
