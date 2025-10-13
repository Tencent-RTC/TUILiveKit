//
//  TUIGiftBulletView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/1/2.
//

import Kingfisher
import TUICore
import UIKit

typealias TUIGiftAnimationCompletionBlock = (Bool) -> Void

class TUIGiftBulletView: UIView {
    var isAnimationPlaying: Bool = false
    var completionBlock: TUIGiftAnimationCompletionBlock?
    var giftData: TUIGiftData? {
        didSet {
            guard let giftData = giftData else { return }
            setGiftData(giftData)
        }
    }

    private let avatarView: UIImageView = {
        let view = UIImageView(frame: CGRect(x: 5, y: 5, width: 40, height: 40))
        view.layer.masksToBounds = true
        view.layer.cornerRadius = view.mm_h * 0.5
        return view
    }()

    private let giftIconView: UIImageView = {
        let view = UIImageView(frame: CGRect(x: -200, y: 5, width: 40, height: 40))
        view.layer.masksToBounds = true
        view.layer.cornerRadius = view.mm_h * 0.5
        return view
    }()

    private lazy var nickNameLabel: UILabel = {
        let label = UILabel(frame: CGRect(x: self.avatarView.mm_w + 15, y: 5, width: 0, height: 20))
        label.font = UIFont.systemFont(ofSize: 14)
        label.textAlignment = .left
        label.lineBreakMode = .byTruncatingMiddle
        label.textColor = .g7
        return label
    }()

    private lazy var giveDescLabel: UILabel = {
        let label = UILabel(frame: CGRect(x: self.avatarView.mm_w + 15, y: 25, width: 0, height: 20))
        label.font = UIFont.systemFont(ofSize: 12)
        label.textAlignment = .left
        label.lineBreakMode = .byTruncatingMiddle
        label.textColor = .lightGrayColor
        return label
    }()

    override init(frame: CGRect) {
        super.init(frame: frame)
        mm_h = 50
        setupUI()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    func setGiftData(_ giftData: TUIGiftData) {
        let userID = giftData.sender.userID
        var nickName = giftData.sender.userName.isEmpty ? userID : giftData.sender.userName
        let avatarUrl = giftData.sender.avatarURL
        if userID == (TUILogin.getUserID() ?? "") {
            nickName = .meText
        } else {
            if nickName.count == 0 {
                nickName = userID
            }
        }

        let maxWidth = UIScreen.main.bounds.size.width / 2
        nickNameLabel.text = nickName
        nickNameLabel.sizeToFit()
        nickNameLabel.mm_w = min(nickNameLabel.mm_w, maxWidth)
        giveDescLabel.text = giftData.giftInfo.name
        giveDescLabel.mm_w = min(giveDescLabel.mm_w, maxWidth)
        giveDescLabel.sizeToFit()

        let width = max(giveDescLabel.mm_w, nickNameLabel.mm_w)
        mm_w = avatarView.mm_w + width + giftIconView.mm_w + 30
        giftIconView.kf.setImage(with: URL(string: giftData.giftInfo.iconURL))
        avatarView.kf.setImage(with: URL(string: avatarUrl))
    }

    func play(isPureMode: Bool, completion: @escaping TUIGiftAnimationCompletionBlock) {
        if !isAnimationPlaying {
            isAnimationPlaying = true
            completionBlock = completion
            begin(isPureMode: isPureMode)
        }
    }

    func stop() {
        giftIconView.layer.removeAllAnimations()
        layer.removeAllAnimations()
        alpha = 0
        completionBlock?(false)
    }

    private func begin(isPureMode: Bool) {
        if isPureMode {
            layer.position.x = mm_w * 0.5 + 20
            layer.opacity = 1
            giftIconView.layer.position.x = mm_w - giftIconView.mm_w * 0.5 - 5
            DispatchQueue.main.asyncAfter(deadline: .now() + 1.0) { [weak self] in
                guard let self = self else { return }
                layer.removeAllAnimations()
                giftIconView.layer.removeAllAnimations()
                alpha = 0
                completionBlock?(true)
            }
        } else {
            let contentAnimation = CAKeyframeAnimation(keyPath: "position.x")
            contentAnimation.values = [NSNumber(value: Float(-mm_w * 0.5)),
                                       NSNumber(value: Float(mm_w * 0.5 + 40)),
                                       NSNumber(value: Float(mm_w * 0.5 + 20)),]
            contentAnimation.duration = 0.25
            contentAnimation.delegate = self
            contentAnimation.fillMode = .forwards
            contentAnimation.isRemovedOnCompletion = false
            
            let opacity = CAKeyframeAnimation(keyPath: "opacity")
            opacity.values = [NSNumber(value: 0.6), NSNumber(value: 1)]
            opacity.calculationMode = .linear
            opacity.fillMode = .forwards
            opacity.isRemovedOnCompletion = false
            opacity.duration = 0.1
            
            layer.add(contentAnimation, forKey: "tui_anim_begin.x")
            layer.add(opacity, forKey: "tui_anim_begin.opacity")
        }
    }

    private func giftIconEnter() {
        let contentAnimation = CAKeyframeAnimation(keyPath: "position.x")
        contentAnimation.values = [NSNumber(value: Float(-mm_w * 0.5)),
                                   NSNumber(value: Float(mm_w - giftIconView.mm_w * 0.5 - 5)),]
        contentAnimation.duration = 0.25
        contentAnimation.delegate = self
        contentAnimation.fillMode = .forwards
        contentAnimation.isRemovedOnCompletion = false

        giftIconView.layer.add(contentAnimation, forKey: "tui_anim_begin.x")
    }

    private func dismiss() {
        let contentAnimation = CAKeyframeAnimation(keyPath: "position.y")
        contentAnimation.values = [NSNumber(value: Float(frame.origin.y)),
                                   NSNumber(value: Float(frame.origin.y - mm_h * 1.5)),]
        contentAnimation.duration = 0.25
        contentAnimation.delegate = self
        contentAnimation.fillMode = .forwards
        contentAnimation.isRemovedOnCompletion = false

        let opacity = CAKeyframeAnimation(keyPath: "opacity")
        opacity.values = [NSNumber(value: 1), NSNumber(value: 0)]
        opacity.duration = 0.25
        opacity.calculationMode = .linear
        opacity.fillMode = .forwards
        opacity.isRemovedOnCompletion = false

        let animationGroup = CAAnimationGroup()
        animationGroup.animations = [contentAnimation, opacity]
        animationGroup.fillMode = .forwards
        animationGroup.isRemovedOnCompletion = false
        animationGroup.delegate = self
        layer.add(animationGroup, forKey: "tui_anim_begin.y")
    }
}

// MARK: Layout

extension TUIGiftBulletView {
    func setupUI() {
        clipsToBounds = true
        layer.masksToBounds = true
        layer.cornerRadius = mm_h * 0.5
        backgroundColor = .g2.withAlphaComponent(0.4)
        addSubview(avatarView)
        addSubview(giftIconView)
        addSubview(nickNameLabel)
        addSubview(giveDescLabel)
    }
}

// MARK: CAAnimationDelegate

extension TUIGiftBulletView: CAAnimationDelegate {
    func animationDidStop(_ anim: CAAnimation, finished flag: Bool) {
        if !flag {
            layer.removeAllAnimations()
            giftIconView.layer.removeAllAnimations()
            alpha = 0
            completionBlock?(true)
        }
        if layer.animation(forKey: "tui_anim_begin.x") == anim {
            layer.removeAllAnimations()
            if flag {
                giftIconEnter()
            }
        } else if giftIconView.layer.animation(forKey: "tui_anim_begin.x") == anim {
            DispatchQueue.main.asyncAfter(deadline: .now() + 1.0) { [weak self] in
                guard let self = self else { return }
                self.dismiss()
            }
        } else if layer.animation(forKey: "tui_anim_begin.y") == anim {
            layer.removeAllAnimations()
            giftIconView.layer.removeAllAnimations()
            alpha = 0
            completionBlock?(true)
        }
    }
}

//MARK: localized String

private extension String {
    static let meText = internalLocalized("Me")
}
