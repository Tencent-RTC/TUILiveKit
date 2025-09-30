//
//  SongListCell.swift
//  Pods
//
//  Created by ssc on 2025/8/23.
//
import UIKit
import RTCRoomEngine

class SongListCell: UITableViewCell {
    private lazy var coverImageView: UIImageView = {
        let imageView = UIImageView()
        imageView.contentMode = .scaleAspectFill
        imageView.layer.cornerRadius = 4
        imageView.clipsToBounds = true
        return imageView
    }()

    var isOwner: Bool = true

    private lazy var titleLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont(name: "PingFangSC-Medium", size: 14)
        label.textColor = UIColor(white: 1, alpha: 0.9)
        return label
    }()

    private lazy var artistLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont.systemFont(ofSize: 12, weight: .regular)
        label.textColor = UIColor(white: 1, alpha: 0.55)
        return label
    }()

    private lazy var actionButton: UIButton = {
        let button = UIButton()
        button.backgroundColor = UIColor("1c66E5")
        button.layer.cornerRadius = 12
        button.setTitle(.SongText, for: .normal)
        button.titleLabel?.font = UIFont.systemFont(ofSize: 12, weight: .medium)
        button.setTitleColor(.white, for: .normal)
        button.addTarget(self, action: #selector(selectSongButtonTapped), for: .touchUpInside)
        return button
    }()

    private lazy var originalButton: UIButton = {
        let button = UIButton()
        button.backgroundColor = UIColor("4086FF").withAlphaComponent(0.1)
        button.setTitle(.originalText, for: .normal)
        button.titleLabel?.font = UIFont(name: "Roboto", size: 10) ?? UIFont.systemFont(ofSize: 10)
        button.setTitleColor(UIColor("4086FF"), for: .normal)
        button.layer.cornerRadius = 4
        return button
    }()

    private lazy var scoreButton: UIButton = {
        let button = UIButton()
        button.backgroundColor = UIColor("38A673").withAlphaComponent(0.1)
        button.setTitle(.scoreText, for: .normal)
        button.titleLabel?.font = UIFont(name: "Roboto", size: 10) ?? UIFont.systemFont(ofSize: 10)
        button.setTitleColor(UIColor("38A673"), for: .normal)
        button.layer.cornerRadius = 4
        return button
    }()

    private var musicId: String = ""
    weak var karaokeManager: KaraokeManager?

    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        setupViews()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private func setupViews() {
        contentView.addSubview(coverImageView)
        contentView.addSubview(titleLabel)
        contentView.addSubview(artistLabel)
        contentView.addSubview(actionButton)
        contentView.addSubview(originalButton)
        contentView.addSubview(scoreButton)

        coverImageView.snp.makeConstraints { make in
            make.left.equalToSuperview().offset(16.scale375())
            make.centerY.equalToSuperview()
            make.width.height.equalTo(48.scale375())
        }

        titleLabel.snp.makeConstraints { make in
            make.left.equalTo(coverImageView.snp.right).offset(12.scale375())
            make.top.equalTo(coverImageView.snp.top)
            make.left.equalTo(coverImageView.snp.right).offset(10.scale375())
        }

        artistLabel.snp.makeConstraints { make in
            make.bottom.equalToSuperview().offset(-10.scale375())
            make.left.equalTo(coverImageView.snp.right).offset(10.scale375())
        }

        actionButton.snp.makeConstraints { make in
            make.right.equalToSuperview().offset(-16.scale375())
            make.centerY.equalToSuperview()
            make.width.equalTo(56.scale375())
            make.height.equalTo(24.scale375())
        }

        originalButton.snp.makeConstraints { make in
            make.left.equalTo(artistLabel.snp.right).offset(6.scale375())
            make.top.equalTo(artistLabel.snp.top)
            make.height.equalTo(16.scale375())
        }

        scoreButton.snp.makeConstraints { make in
            make.left.equalTo(originalButton.snp.right).offset(4.scale375())
            make.top.equalTo(artistLabel.snp.top)
            make.height.equalTo(16.scale375())
        }

        backgroundColor = UIColor("1F2024")
        selectionStyle = .none
    }

    func configure(with song: MusicInfo, indexPath: IndexPath, isSelected: Bool) {
        titleLabel.text = song.musicName
        artistLabel.text = song.artist
        if song.coverUrl == "" {
            coverImageView.image = UIImage.atomicXBundleImage(named: "ktv_coverUrl")
        } else {
            coverImageView.kf.setImage(with: URL(string: song.coverUrl), placeholder: UIImage.avatarPlaceholderImage)
        }
        actionButton.tag = indexPath.row
        self.musicId = song.musicId

        originalButton.snp.removeConstraints()
        scoreButton.snp.removeConstraints()
        
        if song.isOriginal && song.hasRating {
            originalButton.isHidden = false
            scoreButton.isHidden = false
            originalButton.snp.makeConstraints { make in
                make.left.equalTo(artistLabel.snp.right).offset(6.scale375())
                make.top.equalTo(artistLabel.snp.top)
                make.height.equalTo(16.scale375())
            }
            scoreButton.snp.makeConstraints { make in
                make.left.equalTo(originalButton.snp.right).offset(6.scale375())
                make.top.equalTo(artistLabel.snp.top)
                make.height.equalTo(16.scale375())
            }
        } else if song.hasRating {
            originalButton.isHidden = true
            scoreButton.isHidden = false
            scoreButton.snp.makeConstraints { make in
                make.left.equalTo(artistLabel.snp.right).offset(6.scale375())
                make.top.equalTo(artistLabel.snp.top)
                make.height.equalTo(16.scale375())
            }
        } else {
            originalButton.isHidden = true
            scoreButton.isHidden = true
        }
        
        if !isOwner {
            actionButton.isHidden = true
            return
        }

        updateActionButtonState(isSelected: isSelected)
        layoutIfNeeded()
    }

    private func updateActionButtonState(isSelected: Bool) {
        if isSelected {
            actionButton.setTitle(.orderedText, for: .normal)
            actionButton.backgroundColor = .clear
            actionButton.layer.borderWidth = 1
            actionButton.layer.borderColor = UIColor("3A3C42").cgColor
            actionButton.setTitleColor(UIColor(white: 1, alpha: 0.5), for: .normal)
            actionButton.removeTarget(self, action: #selector(selectSongButtonTapped), for: .touchUpInside)
        } else {
            actionButton.setTitle(.SongText, for: .normal)
            actionButton.backgroundColor = UIColor("1c66E5")
            actionButton.layer.borderWidth = 0
            actionButton.setTitleColor(.white, for: .normal)
            actionButton.addTarget(self, action: #selector(selectSongButtonTapped), for: .touchUpInside)
        }
    }

    @objc private func selectSongButtonTapped() {
        guard let karaokeManager = karaokeManager else {return}
        let userInfo = TUIRoomEngine.getSelfInfo()
        let musicInfo = SelectedMusicInfo(
            musicId: musicId,
            userId: userInfo.userId,
            userName: userInfo.userName,
            avatarUrl: userInfo.avatarUrl
        )
        karaokeManager.addSong(selectedMusic: musicInfo)
    }
}

fileprivate extension String {
    static var orderedText: String = ("Ordered").localized
    static var orderedCountText: String = ("Ordered(xxx)").localized
    static var exitOrder: String = ("Exit Order").localized
    static var SongText: String = ("Song").localized
    static var originalText: String = ("Original").localized
    static var scoreText: String = ("Score").localized
}

