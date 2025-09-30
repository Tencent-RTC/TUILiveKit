//
//  SelectedSongCell.swift
//  Pods
//
//  Created by ssc on 2025/8/21.
//
import UIKit
import SnapKit
import Combine
import Kingfisher
import RTCCommon

class SelectedSongCell: UITableViewCell {
    private let indexLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont.systemFont(ofSize: 14, weight: .medium)
        label.textColor = UIColor(white: 1, alpha: 0.9)
        label.textAlignment = .center
        return label
    }()

    private let playingIndicator: UIImageView = {
        let imageView = UIImageView()
        imageView.contentMode = .scaleAspectFit
        imageView.image = UIImage.atomicXBundleImage(named: "ktv_playing_indicator")
        imageView.isHidden = true
        return imageView
    }()

    private let coverImageView: UIImageView = {
        let imageView = UIImageView()
        imageView.contentMode = .scaleAspectFill
        imageView.layer.cornerRadius = 4
        imageView.clipsToBounds = true
        return imageView
    }()

    private let titleLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont.systemFont(ofSize: 14, weight: .medium)
        label.textColor = UIColor(white: 1, alpha: 0.9)
        return label
    }()

    private let userNameLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont(name: "PingFangSC-Regular", size: 12)
        label.textColor = UIColor(white: 1, alpha: 0.55)
        label.textAlignment = .center
        label.baselineAdjustment = .alignCenters
        return label
    }()

    private let avatarImage: UIImageView = {
        let view = UIImageView()
        view.layer.cornerRadius = 14.scale375()/2
        view.layer.masksToBounds = true
        return view
    }()

    private let topButton: UIButton = {
        let button = UIButton()
        button.setImage(UIImage.atomicXBundleImage(named: "ktv_top"), for: .normal)
        button.isHidden = true
        return button
    }()

    private let removeButton: UIButton = {
        let button = UIButton()
        button.setImage(UIImage.atomicXBundleImage(named: "ktv_delete"), for: .normal)
        return button
    }()

    private let playPauseButton: UIButton = {
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

    private var musicId: String = ""
    var isOwner: Bool = false
    weak var karaokeManager: KaraokeManager?
    private var cancellables = Set<AnyCancellable>()

    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
    }
        
    deinit {
        cancellables.forEach { $0.cancel() }
        cancellables.removeAll()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private func constructViewHierarchy() {
        contentView.addSubview(indexLabel)
        contentView.addSubview(playingIndicator)
        contentView.addSubview(coverImageView)
        contentView.addSubview(titleLabel)
        contentView.addSubview(userNameLabel)
        contentView.addSubview(avatarImage)
        contentView.addSubview(topButton)
        contentView.addSubview(removeButton)
        contentView.addSubview(playPauseButton)
        contentView.addSubview(nextButton)
    }

    private func activateConstraints() {
        indexLabel.snp.makeConstraints { make in
            make.left.equalToSuperview().offset(16)
            make.centerY.equalToSuperview()
            make.width.equalTo(20)
        }

        playingIndicator.snp.makeConstraints { make in
            make.left.equalToSuperview().offset(16)
            make.centerY.equalToSuperview()
            make.width.height.equalTo(16)
        }

        coverImageView.snp.makeConstraints { make in
            make.left.equalTo(indexLabel.snp.right).offset(8)
            make.centerY.equalToSuperview()
            make.width.height.equalTo(48)
        }

        titleLabel.snp.makeConstraints { make in
            make.left.equalTo(coverImageView.snp.right).offset(12)
            make.top.equalTo(coverImageView.snp.top)
            make.right.lessThanOrEqualToSuperview().offset(-10)
        }

        avatarImage.snp.makeConstraints { make in
            make.left.equalTo(titleLabel)
            make.top.equalTo(titleLabel.snp.bottom).offset(6.scale375())
            make.width.height.equalTo(14.scale375())
        }

        userNameLabel.snp.makeConstraints { make in
            make.left.equalTo(avatarImage.snp.right).offset(4.scale375())
            make.top.equalTo(avatarImage.snp.top)
            make.bottom.equalToSuperview().offset(-16)
        }

        backgroundColor = UIColor("1F2024")
        selectionStyle = .none
    }

    private func bindInteraction() {
        topButton.addTarget(self, action: #selector(topButtonTapped), for: .touchUpInside)
        removeButton.addTarget(self, action: #selector(removeButtonTapped), for: .touchUpInside)
        playPauseButton.addTarget(self, action: #selector(playPauseButtonTapped), for: .touchUpInside)
        nextButton.addTarget(self, action: #selector(nextButtonTapped), for: .touchUpInside)
    }

    func configure(with song: MusicInfo, at index: Int) {
        titleLabel.text = song.musicName
        if karaokeManager?.karaokeState.selectedSongs[index].userName == "" {
            userNameLabel.text = karaokeManager?.karaokeState.selectedSongs[index].userId
        } else {
            userNameLabel.text = karaokeManager?.karaokeState.selectedSongs[index].userName
        }
        if song.coverUrl == "" {
            coverImageView.image = UIImage.atomicXBundleImage(named: "ktv_coverUrl")
        } else {
            coverImageView.kf.setImage(with: URL(string: song.coverUrl), placeholder: UIImage.avatarPlaceholderImage)
        }
        avatarImage.kf.setImage(with: URL(string: karaokeManager?.karaokeState.selectedSongs[index].avatarUrl ?? ""), placeholder: UIImage.avatarPlaceholderImage)

        self.musicId = song.musicId
        if index == 0 {
            if isOwner {
                topButton.isHidden = true
                removeButton.isHidden = true
                indexLabel.isHidden = true
                playingIndicator.isHidden = false
                playPauseButton.isHidden = false
                nextButton.isHidden = false

                karaokeManager?.subscribe(StateSelector(keyPath: \.playbackState))
                    .receive(on: DispatchQueue.main)
                    .sink { [weak self] _ in
                        guard let self = self else {return}
                        self.updatePlayPauseButtonState()
                    }
                    .store(in: &cancellables)

                nextButton.snp.remakeConstraints { make in
                    make.right.equalToSuperview().offset(-16.scale375())
                    make.centerY.equalToSuperview()
                    make.width.height.equalTo(16.scale375())
                }

                playPauseButton.snp.remakeConstraints { make in
                    make.right.equalTo(nextButton.snp.left).offset(-16.scale375())
                    make.centerY.equalToSuperview()
                    make.width.height.equalTo(16.scale375())
                }
            } else {
                indexLabel.isHidden = true
                playingIndicator.isHidden = false
            }
        } else if index == 1 {
            if isOwner {
                topButton.isHidden = true
                playPauseButton.isHidden = true
                nextButton.isHidden = true
                indexLabel.isHidden = false
                playingIndicator.isHidden = true
                removeButton.isHidden = false
                indexLabel.text = "\(index + 1)"

                removeButton.snp.remakeConstraints { make in
                    make.right.equalToSuperview().offset(-16.scale375())
                    make.centerY.equalToSuperview()
                    make.width.height.equalTo(16.scale375())
                }
            } else {
                indexLabel.isHidden = false
                playingIndicator.isHidden = true
                indexLabel.text = "\(index + 1)"
            }
        } else {
            if isOwner {
                topButton.isHidden = true
                playPauseButton.isHidden = true
                nextButton.isHidden = true
                removeButton.isHidden = false
                indexLabel.isHidden = false
                playingIndicator.isHidden = true
                indexLabel.text = "\(index + 1)"
                topButton.isHidden = false
                topButton.tag = index
                removeButton.snp.remakeConstraints { make in
                    make.right.equalToSuperview().offset(-16.scale375())
                    make.centerY.equalToSuperview()
                    make.width.height.equalTo(16.scale375())
                }

                topButton.snp.remakeConstraints { make in
                    make.right.equalTo(removeButton.snp.left).offset(-16.scale375())
                    make.centerY.equalToSuperview()
                    make.width.height.equalTo(16.scale375())
                }
            } else {
                playingIndicator.isHidden = true
                indexLabel.isHidden = false
                indexLabel.text = "\(index + 1)"
            }
        }
    }

    @objc private func updatePlayPauseButtonState() {
        guard let karaokeManager = karaokeManager else { return }
        let isPaused = karaokeManager.karaokeState.playbackState == .pause
        playPauseButton.setImage(
            isPaused ? UIImage.atomicXBundleImage(named: "ktv_resume") : UIImage.atomicXBundleImage(named: "ktv_pause"),
            for: .normal
        )
    }

    @objc private func topButtonTapped() {
        guard let karaokeManager = karaokeManager else {return}
        karaokeManager.prioritizeMusic(musicId: self.musicId)
    }

    @objc private func removeButtonTapped() {
        guard let karaokeManager = karaokeManager else {return}
        karaokeManager.eraseMusic(musicId: musicId)
    }

    @objc private func playPauseButtonTapped() {
        guard let karaokeManager = karaokeManager else {return}
        if karaokeManager.karaokeState.playbackState == .pause {
            karaokeManager.resumePlayback()
        } else if karaokeManager.karaokeState.playbackState == .resume || karaokeManager.karaokeState.playbackState == .start{
            karaokeManager.pausePlayback()
        }
    }

    @objc private func nextButtonTapped() {
        guard let karaokeManager = karaokeManager else {return}
        karaokeManager.playNextMusic()
    }
}
