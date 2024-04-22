//
//  TUIGiftPlayView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/1/2.
//

import SVGAPlayer
import TUICore
import UIKit

protocol TUIGiftPlayViewDelegate: AnyObject {
    func giftPlayView(_ giftPlayView: TUIGiftPlayView, onReceiveGift gift: TUIGift, giftCount: Int, sender: TUIGiftUser, receiver: TUIGiftUser)
    func giftPlayView(_ giftPlayView: TUIGiftPlayView, onPlayGiftAnimation gift: TUIGift)
}

let gLikeMaxAnimationCount: Int = 10
let Screen_Width = UIScreen.main.bounds.size.width
let Screen_Height = UIScreen.main.bounds.size.height
let Bottom_SafeHeight = WindowUtils.bottomSafeHeight

class TUIGiftPlayView: UIView {
    weak var delegate: TUIGiftPlayViewDelegate?
    private var groupId: String = ""
    private var presenter: TUIGiftPresenter = TUIGiftPresenter()
    private var currentLikeAnimationCount: Int = 0
    private var likeCount = 0
    private var giftCacheKey = ""

    private lazy var animationManager: TUIGiftAnimationManager = {
        let manager = TUIGiftAnimationManager(simulcastCount: 99)
        manager.dequeueClosure = { [weak self] giftData in
            guard let self = self else { return }
            self.showViewAnimation(giftData)
        }
        return manager
    }()

    private lazy var svgaManager: TUIGiftAnimationManager = {
        let manager = TUIGiftAnimationManager(simulcastCount: 1)
        manager.dequeueClosure = { [weak self] giftData in
            guard let self = self else { return }
            self.giftCacheKey = giftData.gift.giftId
            self.showSVGAAnimation(giftData: giftData)
        }
        return manager
    }()

    private let likeColors: [UIColor] = [.red, .purple, .orange,
                                 .yellow, .green, .blue,
                                 .gray, .cyan, .brown,]

    init(groupId: String) {
        self.groupId = groupId
        super.init(frame: .zero)
        isUserInteractionEnabled = false
        initPresenter()
        addObserver()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private func addObserver() {
        TUIGiftStore.shared.giftData.addObserver(self) { [weak self] giftData, _ in
            guard let self = self else { return }
            self.delegate?.giftPlayView(self,
                                        onReceiveGift: giftData.gift,
                                        giftCount: giftData.giftCount,
                                        sender: giftData.sender,
                                        receiver: giftData.receiver)
            self.playGiftModel(giftModel: giftData.gift,
                               sender: giftData.sender,
                               receiver: giftData.receiver,
                               giftCount: giftData.giftCount)
        }
        TUIGiftStore.shared.likeData.addObserver(self) { [weak self] likeData, _ in
            guard let self = self else { return }
            self.playLikeModel(sender: likeData.sender)
        }
    }
    
    private func initPresenter() {
        presenter = TUIGiftPresenter.defaultCreate(self, groupId: groupId)
    }
    
    private func clearData() {
        animationManager.clearData()
        svgaManager.clearData()
    }

    private func playGiftModel(giftModel: TUIGift, sender: TUIGiftUser, receiver: TUIGiftUser, giftCount: Int) {
        if giftModel.animationUrl.count > 0 {
            svgaManager.enqueue(giftModel: giftModel, sender: sender, receiver: receiver, giftCount: giftCount)
        } else {
            animationManager.enqueue(giftModel: giftModel, sender: sender, receiver: receiver, giftCount: giftCount)
        }
    }

    private func playLikeModel(sender: TUIGiftUser) {
        if currentLikeAnimationCount >= gLikeMaxAnimationCount {
            return
        }

        let startFrame = CGRect(x: (Screen_Width * 5) / 6, y: Screen_Height - Bottom_SafeHeight - 10 - 44, width: 44, height: 44)
        let heartImageView = UIImageView(frame: startFrame)
        heartImageView.image = .liveBundleImage("live_gift_like_icon")

        heartImageView.tintColor = likeColors[Int(arc4random()) % likeColors.count]
        addSubview(heartImageView)
        heartImageView.alpha = 0
        heartImageView.layer.add(likeAnimation(startFrame), forKey: nil)
        currentLikeAnimationCount += 1

        DispatchQueue.main.asyncAfter(deadline: .now() + 3) { [weak self] in
            heartImageView.removeFromSuperview()
            self?.currentLikeAnimationCount -= 1
        }
    }

    private func showViewAnimation(_ giftData: TUIGiftData) {
        let beginY = mm_h * 0.5
        for view in subviews {
            if let bulletView = view as? TUIGiftBulletView {
                UIView.animate(withDuration: 0.1) {
                    bulletView.mm_y = bulletView.mm_y - (bulletView.mm_h + 10)
                }
                if (beginY - (bulletView.mm_h + 10) * 2) > bulletView.mm_y {
                    bulletView.stop()
                }
            }
        }
        let all = TUIGiftBulletView(frame: CGRect.zero)
        all.mm_y = beginY
        all.mm_x = 20
        all.giftData = giftData
        addSubview(all)

        all.play { [weak self, weak all] _ in
            guard let self = self else { return }
            all?.removeFromSuperview()
            self.animationManager.finishPlay()
        }
    }

    private func showSVGAAnimation(giftData: TUIGiftData) {
        delegate?.giftPlayView(self, onPlayGiftAnimation: giftData.gift)
    }
    
    deinit {
        removeObserver()
    }
    
    private func removeObserver() {
        TUIGiftStore.shared.giftData.removeObserver(self)
        TUIGiftStore.shared.likeData.removeObserver(self)
    }
    
    func playGiftAnimation(animationData: Data) {
        let playerView = SVGAPlayer(frame: bounds)
        playerView.contentMode = .scaleAspectFit
        addSubview(playerView)
        playerView.delegate = self
        playerView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        playerView.loops = 1
        playerView.clearsAfterStop = true
        let parser = SVGAParser()
        parser.parse(with: animationData, cacheKey: giftCacheKey) { videoItem in
            playerView.videoItem = videoItem
            playerView.startAnimation()
        } failureBlock: { [weak self] _ in
            guard let self = self else { return }
            self.svgaPlayerDidFinishedAnimation(playerView)
        }

    }
    
    func getLikeCount() -> Int {
        return likeCount
    }
}

// MARK: Like Animation

private extension TUIGiftPlayView {
    private func likeAnimation(_ frame: CGRect) -> CAAnimation {
        let opacityAnimation = CABasicAnimation(keyPath: "opacity")
        opacityAnimation.fromValue = 1.0
        opacityAnimation.toValue = 0
        opacityAnimation.isRemovedOnCompletion = false
        opacityAnimation.beginTime = 0.0
        opacityAnimation.duration = 3.0

        let scaleAnimation = CABasicAnimation(keyPath: "transform.scale")
        scaleAnimation.fromValue = 0.0
        scaleAnimation.toValue = 1.0
        scaleAnimation.isRemovedOnCompletion = false
        scaleAnimation.fillMode = CAMediaTimingFillMode.forwards
        scaleAnimation.duration = 0.5

        let positionAnimation = CAKeyframeAnimation(keyPath: "position")
        positionAnimation.beginTime = 0.5
        positionAnimation.duration = 2.5
        positionAnimation.fillMode = CAMediaTimingFillMode.forwards
        positionAnimation.calculationMode = CAAnimationCalculationMode.cubicPaced
        positionAnimation.path = likeAnimationPositionPath(frame).cgPath

        let animationGroup = CAAnimationGroup()
        animationGroup.animations = [opacityAnimation, scaleAnimation, positionAnimation]
        animationGroup.duration = 3.0

        return animationGroup
    }

    private func likeAnimationPositionPath(_ frame: CGRect) -> UIBezierPath {
        let path = UIBezierPath()

        let point0 = CGPoint(x: frame.origin.x + frame.size.width / 2, y: frame.origin.y + frame.size.height / 2)
        let randomX1 = CGFloat.random(in: -30 ... 30)
        let randomY1 = CGFloat.random(in: -60 ... 0)
        let point1 = CGPoint(x: point0.x + randomX1, y: frame.origin.y + randomY1)

        let randomX2 = CGFloat.random(in: -15 ... 15)
        let randomY2 = CGFloat.random(in: -60 ... 0)
        let point2 = CGPoint(x: point0.x + randomX2, y: frame.origin.y + randomY2)

        let pointOffset3 = UIScreen.main.bounds.width * 0.1
        let pointOffset4 = UIScreen.main.bounds.width * 0.2

        let randomX4 = CGFloat.random(in: -pointOffset4 ... pointOffset4)
        let randomY4 = CGFloat.random(in: 240 ... 270)
        let point4 = CGPoint(x: point0.x + randomX4, y: randomY4)

        let randomX3 = CGFloat.random(in: pointOffset3 ... pointOffset3 * 2)
        let randomY3 = CGFloat.random(in: -30 ... 30)
        let point3 = CGPoint(x: point0.x + randomX3, y: (point4.y + point2.y) / 2 + randomY3)

        path.move(to: point0)
        path.addQuadCurve(to: point2, controlPoint: point1)
        path.addQuadCurve(to: point4, controlPoint: point3)

        return path
    }
}

extension TUIGiftPlayView: SVGAPlayerDelegate {
    func svgaPlayerDidFinishedAnimation(_ player: SVGAPlayer!) {
        UIView.animate(withDuration: 0.2) {
            player.alpha = 0
        } completion: { _ in
            player.removeFromSuperview()
        }
        svgaManager.finishPlay()
    }
}


// MARK: TUIGiftPresenterDelegate

extension TUIGiftPlayView: TUIGiftPresenterDelegate {
    func onGiftDidSend(_ model: TUIGift, sender: TUIGiftUser, receiver: TUIGiftUser, giftCount: Int, isSuccess: Bool, message: String) {
    }

    func onLikeDidSend(sender: TUIGiftUser, isSuccess: Bool, message: String) {
        
    }

    func onReceiveGift(_ model: TUIGift, giftCount: Int, sender: TUIGiftUser, receiver: TUIGiftUser) {
        playGiftModel(giftModel: model, sender: sender, receiver: receiver, giftCount: giftCount)
        delegate?.giftPlayView(self, onReceiveGift: model, giftCount: giftCount, sender: sender, receiver: receiver)
    }

    func onReceiveLike(sender: TUIGiftUser) {
        playLikeModel(sender: sender)
        likeCount += 1
    }
}
