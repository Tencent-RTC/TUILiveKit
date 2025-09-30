//
//  ScoreBoardView.swift
//  Pods
//
//  Created by ssc on 2025/8/27.
//
import UIKit
import RTCCommon

class ScoreBoardView: UIView {
    private lazy var scoreLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont(name: "DIN-Bold", size: 40) ?? .boldSystemFont(ofSize: 40)
        label.textAlignment = .center
        label.textColor = UIColor("FF6A4C")
        label.text = "75.1"
        return label
    }()

    private lazy var unitLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont(name: "PingFangSC-Medium", size: 16) ?? .systemFont(ofSize: 16, weight: .medium)
        label.textAlignment = .right
        label.textColor = UIColor("FF6A4C")
        label.text = .pointText
        return label
    }()

    private lazy var avatarImageView: UIImageView = {
        let imageView = UIImageView()
        imageView.contentMode = .scaleAspectFill
        imageView.clipsToBounds = true
        imageView.layer.cornerRadius = 14
        imageView.image = UIImage.atomicXBundleImage(named: "ktv_note")
        return imageView
    }()

    private lazy var nameLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont(name: "PingFangSC-Regular", size: 12)
        label.textColor = UIColor.white.withAlphaComponent(0.55)
        label.text = "1212"
        return label
    }()

    private lazy var textLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont(name: "PingFangSC-Regular", size: 10)
        label.textColor = UIColor.white.withAlphaComponent(0.55)
        label.text = .songingScoreText
        return label
    }()

    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        isViewReady = true
    }

    private func constructViewHierarchy() {
        addSubview(scoreLabel)
        addSubview(unitLabel)
        addSubview(avatarImageView)
        addSubview(nameLabel)
        addSubview(textLabel)
    }

    private func activateConstraints() {
        scoreLabel.snp.makeConstraints{ make in
            make.height.equalTo(43.scale375())
            make.left.equalToSuperview()
            make.top.equalToSuperview()
        }

        unitLabel.snp.makeConstraints{ make in
            make.top.equalTo(scoreLabel.snp.top).offset(15.scale375())
            make.left.equalTo(scoreLabel.snp.right).offset(4.scale375())
            make.height.equalTo(22.scale375())
            make.width.equalTo(16.scale375())
        }

        avatarImageView.snp.makeConstraints{ make in
            make.top.equalTo(scoreLabel.snp.bottom).offset(10.scale375())
            make.left.equalToSuperview()
            make.height.equalTo(14.scale375())
            make.width.equalTo(14.scale375())
        }

        nameLabel.snp.makeConstraints{ make in
            make.top.equalTo(avatarImageView.snp.top)
            make.left.equalTo(avatarImageView.snp.right).offset(5.scale375())
        }

        textLabel.snp.makeConstraints{ make in
            make.left.equalTo(nameLabel.snp.right).offset(9.scale375())
            make.top.equalTo(avatarImageView.snp.top).offset(-2.scale375())
            make.height.equalTo(20.scale375())
        }
    }

    private func bindInteraction() {

    }

    func showScoreBoard(imageURl: String,username: String, score: Float) {
        avatarImageView.kf.setImage(with: URL(string: imageURl), placeholder: UIImage.avatarPlaceholderImage)
        nameLabel.text = username
        scoreLabel.text = String(format: "%.1f", score)
    }
}

fileprivate extension String {
    static var songingScoreText: String = ("Singing Score").localized
    static var pointText: String = ("Points").localized
}
