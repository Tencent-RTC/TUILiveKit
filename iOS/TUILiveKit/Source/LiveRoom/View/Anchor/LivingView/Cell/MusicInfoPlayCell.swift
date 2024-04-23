//
//  MusicInfoPlayCell.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/20.
//

import Foundation

class MusicInfoPlayCell: MusicInfoCell {
    private lazy var startPlayButton: UIButton = {
        let view = UIButton(type: .system)
        view.showsTouchWhenHighlighted = false
        view.setBackgroundImage(.liveBundleImage("live_music_start_play"), for: .normal)
        view.sizeToFit()
        view.addTarget(self, action: #selector(startPlayButtonClick), for: .touchUpInside)
        view.isHidden = true
        return view
    }()

    private lazy var stopPlayButton: UIButton = {
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
            make.leading.equalToSuperview().inset(24.scale375())
            make.trailing.equalToSuperview().inset(100.scale375())
        }

        deleteButton.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.trailing.equalToSuperview().inset(24.scale375())
            make.width.equalTo(deleteButton.mm_w)
            make.height.equalTo(deleteButton.mm_h)
        }

        startPlayButton.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.trailing.equalTo(deleteButton.snp.leading).offset(-16.scale375())
            make.width.equalTo(startPlayButton)
            make.height.equalTo(startPlayButton)
        }

        stopPlayButton.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.trailing.equalTo(deleteButton.snp.leading).offset(-16.scale375())
            make.width.equalTo(stopPlayButton)
            make.height.equalTo(stopPlayButton)
        }

        lineView.snp.makeConstraints { make in
            make.bottom.equalToSuperview()
            make.leading.equalTo(nameLabel)
            make.trailing.equalToSuperview().inset(24)
            make.height.equalTo(1)
        }
    }

    override func updateView() {
        super.updateView()
        guard let musicInfo = musicInfo else { return }
        stopPlayButton.isHidden = !musicInfo.isPlaying.value
        startPlayButton.isHidden = musicInfo.isPlaying.value
    }
}

// MARK: Action

extension MusicInfoPlayCell {
    @objc func startPlayButtonClick() {
        guard let musicInfo = musicInfo else { return }
        action.value = .startPlay(musicInfo: musicInfo)
    }

    @objc func stopPlayButtonClick() {
        guard let musicInfo = musicInfo else { return }
        action.value = .stopPlay(musicInfo: musicInfo)
    }

    @objc func deleteButtonClick() {
        guard let musicInfo = musicInfo else { return }
        action.value = .delete(musicInfo: musicInfo)
    }
}

private extension String {
    static var anchorHangUpTitle: String {
        localized("live.anchor.link.hang.up.title")
    }
}
