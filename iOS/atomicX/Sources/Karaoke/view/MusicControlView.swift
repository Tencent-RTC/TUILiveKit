//
//  MusicControlView.swift
//  Pods
//
//  Created by ssc on 2025/8/14.
//
import UIKit
import SnapKit
import Combine
#if canImport(TXLiteAVSDK_TRTC)
import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
import TXLiteAVSDK_Professional
import RTCCommon
import TUICore
#endif

class MusicControlView: UIView {
    weak var karaokeManager: KaraokeManager?
    var onSongListButtonTapped: (() -> Void)?
    private let isOwner: Bool
    private let isKTV: Bool
    private var totalTime: TimeInterval = 0
    private let backgroundView: UIView = {
        let view = UIView()
        view.backgroundColor = UIColor.black.withAlphaComponent(0.4)
        view.layer.cornerRadius = 16
        view.clipsToBounds = true
        view.isUserInteractionEnabled = true
        return view
    }()

    private let darkBlurView: UIView = {
        let view = UIView()
        return view
    }()

    private let songListButton: UIButton = {
        let button = UIButton()
        button.setImage(UIImage.atomicXBundleImage(named: "ktv_selectSong"), for: .normal)
        return button
    }()

    private let playButton: UIButton = {
        let button = UIButton()
        button.setImage(UIImage.atomicXBundleImage(named: "ktv_resume"), for: .normal)
        button.isHidden = true
        return button
    }()

    private let nextButton: UIButton = {
        let button = UIButton()
        button.setImage(UIImage.atomicXBundleImage(named: "ktv_next"), for: .normal)
        button.isHidden = true
        return button
    }()

    private let regulateButton: UIButton = {
        let button = UIButton()
        button.setImage(UIImage.atomicXBundleImage(named: "ktv_setting"), for: .normal)
        return button
    }()

    private let originalButton: UIButton = {
        let button = UIButton()
        return button
    }()
    
    private let progressLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont(name: "PingFang SC", size: 9)
        label.textColor = UIColor(white: 1, alpha: 0.42)
        label.text = "00:00 / 00:00"
        label.textAlignment = .left
        return label
    }()

    private let musicIcon: UIImageView = {
        let view = UIImageView()
        view.image = UIImage.atomicXBundleImage(named: "ktv_note")
        return view
    }()

    private let titleLabel: UILabel = {
        let label = UILabel()
        label.frame = CGRect(x: 14, y: 0, width: 48, height: 17)
        label.font = UIFont(name: "PingFang SC", size: 12)
        label.textColor = UIColor.white.withAlphaComponent(0.9)
        return label
    }()

    private var cancellables = Set<AnyCancellable>()

    private var isViewReady = false

    init(isOwner: Bool,isKTV: Bool) {
        self.isOwner = isOwner
        self.isKTV = isKTV
        super.init(frame: .zero)
    }

    deinit {
        cancellables.forEach { $0.cancel() }
        cancellables.removeAll()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    func getSongListButtonFrame() -> CGRect {
        return songListButton.frame
    }

    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else {
            return
        }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
    }

    private func constructViewHierarchy() {
        addSubview(musicIcon)
        addSubview(titleLabel)
        addSubview(backgroundView)
        addSubview(songListButton)
        addSubview(originalButton)
        addSubview(regulateButton)
        addSubview(nextButton)
        addSubview(playButton)
        addSubview(progressLabel)
    }

    private func activateConstraints() {
        if isKTV {
            musicIcon.snp.makeConstraints { make in
                make.width.equalTo(7.scale375())
                make.height.equalTo(9.scale375())
                make.left.equalToSuperview().offset(3.scale375())
                make.top.equalToSuperview().offset(8.scale375())
            }

            titleLabel.snp.makeConstraints { make in
                make.left.equalTo(musicIcon.snp.right).offset(2.scale375())
                make.top.equalToSuperview().offset(4.scale375())
            }

            progressLabel.snp.makeConstraints { make in
                make.width.equalTo(61.scale375())
                make.height.equalTo(13.scale375())
                make.left.equalToSuperview()
                make.top.equalTo(musicIcon.snp.top).offset(17.scale375())
            }

            originalButton.snp.makeConstraints { make in
                make.right.equalToSuperview()
                make.top.equalToSuperview().offset(8.scale375())
                make.width.height.equalTo(20.scale375())
            }

            regulateButton.snp.makeConstraints { make in
                make.right.equalTo(originalButton.snp.left).offset(-10.scale375())
                make.top.equalToSuperview().offset(8.scale375())
                make.width.height.equalTo(20.scale375())
            }

            nextButton.snp.makeConstraints { make in
                make.right.equalTo(regulateButton.snp.left).offset(-10.scale375())
                make.top.equalToSuperview().offset(8.scale375())
                make.width.height.equalTo(20.scale375())
            }

            playButton.snp.makeConstraints { make in
                make.right.equalTo(nextButton.snp.left).offset(-10.scale375())
                make.top.equalToSuperview().offset(8.scale375())
                make.width.height.equalTo(20.scale375())
            }

        } else {
            musicIcon.isHidden = true
            titleLabel.isHidden = true
            backgroundView.snp.makeConstraints { make in
                make.right.equalToSuperview()
                make.top.equalToSuperview().offset(6.scale375())
                make.width.equalTo(144.scale375())
                make.height.equalTo(32.scale375())
            }

            songListButton.snp.makeConstraints { make in
                make.right.equalTo(backgroundView.snp.right)
                make.width.height.equalTo(32.scale375())
                make.centerY.equalToSuperview()
            }

            originalButton.snp.makeConstraints { make in
                make.right.equalTo(songListButton.snp.left).offset(-10.scale375())
                make.centerY.equalTo(songListButton).offset(2.scale375())
                make.width.height.equalTo(16.scale375())
            }

            regulateButton.snp.makeConstraints { make in
                make.right.equalTo(originalButton.snp.left).offset(-10.scale375())
                make.centerY.equalTo(songListButton).offset(2.scale375())
                make.width.height.equalTo(16.scale375())
            }

            nextButton.snp.makeConstraints { make in
                make.right.equalTo(regulateButton.snp.left).offset(-10.scale375())
                make.centerY.equalTo(songListButton).offset(2.scale375())
                make.width.height.equalTo(16.scale375())
            }

            playButton.snp.makeConstraints { make in
                make.right.equalTo(nextButton.snp.left).offset(-10.scale375())
                make.centerY.equalTo(songListButton).offset(2.scale375())
                make.width.height.equalTo(16.scale375())
            }
        }
    }

    private func bindInteraction() {
        regulateButton.addTarget(self, action: #selector(onRegulateButtonClicked), for: .touchUpInside)
        nextButton.addTarget(self, action: #selector(onNextButtonClicked), for: .touchUpInside)
        playButton.addTarget(self, action: #selector(onPlayButtonClicked), for: .touchUpInside)
        originalButton.addTarget(self, action: #selector(onOriginalButtonClicked), for: .touchUpInside)
        songListButton.addTarget(self, action: #selector(onSongListButtonClicked), for: .touchUpInside)

        karaokeManager?.subscribe(StateSelector(keyPath: \.playbackState))
            .receive(on: DispatchQueue.main)
            .removeDuplicates()
            .sink { [weak self] _ in
                guard let self = self else {return}
                self.updatePlayPauseButtonState()
            }
            .store(in: &cancellables)

        karaokeManager?.subscribe(StateSelector(keyPath: \.selectedSongs))
            .receive(on: DispatchQueue.main)
            .removeDuplicates()
            .sink { [weak self] selectedSongs in
                guard let self = self else {return}
                if selectedSongs.count == 0 {
                    self.hidden()
                    self.titleLabel.text = .noSongText
                    self.totalTime = 0
                    updateProgress(current: 0)
                } else {
                    guard let karaokeManager = karaokeManager else {return}
                    guard let musicInfo = karaokeManager.karaokeState.songLibrary.first(
                        where: { $0.musicId ==  selectedSongs.first?.musicId
                        }),
                          !karaokeManager.karaokeState.selectedSongs.isEmpty else {
                        return
                    }
                    self.titleLabel.text = "\(musicInfo.musicName) - \(musicInfo.artist)"
                    if karaokeManager.karaokeState.playbackState == .stop {
                        self.progressLabel.isHidden = true
                    }
                    let chorusRole = karaokeManager.karaokeState.chorusRole
                    self.showByChorusRole(chorusRole: chorusRole)
                }
            }
            .store(in: &cancellables)

        karaokeManager?.subscribe(StateSelector(keyPath: \.musicTrackType))
            .receive(on: DispatchQueue.main)
            .removeDuplicates()
            .sink { [weak self] musicTrackType in
                guard let self = self else {return}
                if musicTrackType == .accompaniment {
                    originalButton.setImage(UIImage.atomicXBundleImage(named: "ktv_open_original"), for: .normal)
                } else {
                    originalButton.setImage(UIImage.atomicXBundleImage(named: "ktv_close_original"), for: .normal)
                }
            }
            .store(in: &cancellables)

        karaokeManager?.subscribe(StateSelector(keyPath: \.playProgress))
            .receive(on: DispatchQueue.main)
            .removeDuplicates()
            .dropFirst()
            .sink { [weak self] playProgress in
                guard let self = self else {return}
                updateProgress(current: playProgress)
            }
            .store(in: &cancellables)

        karaokeManager?
            .subscribe(StateSelector(keyPath: \.currentMusicTotalDuration))
            .receive(on: DispatchQueue.main)
            .removeDuplicates()
            .sink { [weak self] currentMusicTotalDuration in
                guard let self = self else {return}
                totalTime = currentMusicTotalDuration
            }
            .store(in: &cancellables)
    }

    private func hidden() {
        nextButton.isHidden = true
        playButton.isHidden = true
        regulateButton.isHidden = true
        originalButton.isHidden = true
        backgroundView.isHidden = true
    }

    private func showByChorusRole(chorusRole: TXChorusRole) {
        let isLeadSinger = (chorusRole == .leadSinger)
        nextButton.isHidden = !isLeadSinger
        playButton.isHidden = !isLeadSinger
        regulateButton.isHidden = !isLeadSinger
        originalButton.isHidden = !isLeadSinger
        backgroundView.isHidden = !isLeadSinger
    }

    private func updateProgress(current: TimeInterval) {
        let currentStr = String(format: "%02d:%02d", Int(current) / 60, Int(current) % 60)
        let totalStr = String(format: "%02d:%02d", Int(totalTime) / 60, Int(totalTime) % 60)
        progressLabel.text = "\(currentStr) / \(totalStr)"
        progressLabel.isHidden = false
    }

    @objc private func updatePlayPauseButtonState() {
        guard let karaokeManager = karaokeManager else { return }
        let isPaused = karaokeManager.karaokeState.playbackState == .pause
        playButton.setImage(
            isPaused ? UIImage.atomicXBundleImage(named: "ktv_resume") : UIImage.atomicXBundleImage(named: "ktv_pause"),
            for: .normal
        )

        if karaokeManager.karaokeState.playbackState == .stop {
            progressLabel.text = "00:00 / 00:00"
        }
    }

    @objc private func onSongListButtonClicked() {
        self.onSongListButtonTapped?()
    }

    @objc private func onNextButtonClicked() {
        guard let karaokeManager = karaokeManager else {return}
        karaokeManager.playNextMusic()
    }
    
    @objc private func onPlayButtonClicked() {
        guard let karaokeManager = karaokeManager else {return}
        if karaokeManager.karaokeState.playbackState == .pause {
            karaokeManager.resumePlayback()
        } else if karaokeManager.karaokeState.playbackState == .resume || karaokeManager.karaokeState.playbackState == .start{
            karaokeManager.pausePlayback()
        }
    }
    
    @objc private func onOriginalButtonClicked() {
        guard let karaokeManager = karaokeManager else { return }
        let newTrackType: TXChorusMusicTrack = karaokeManager.karaokeState.musicTrackType == .accompaniment ? .originalSong : .accompaniment
        karaokeManager.switchMusicTrack(trackType: newTrackType)
    }

    @objc private func onRegulateButtonClicked() {
        guard let karaokeManager = karaokeManager else { return }
        guard let vc = WindowUtils.getCurrentWindowViewController() else { return }
        let songListView = MusicSettingViewController(karaokeManager: karaokeManager)
        vc.present(songListView, animated: true)
    }

    @objc private func didTapSongListButton() {
        self.onSongListButtonTapped?()
    }

    override func layoutSubviews() {
        super.layoutSubviews()
    }
}


fileprivate extension String {
    static var noSongText: String = ("No songs").localized
}
