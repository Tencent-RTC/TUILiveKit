//
//  GiftPlayView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/1/2.
//

import SVGAPlayer
import TUICore
import UIKit
import RTCCommon

protocol GiftPlayViewDelegate: AnyObject {
    func giftPlayView(_ giftPlayView: GiftPlayView, onReceiveGift gift: TUIGift, giftCount: Int, sender: TUIGiftUser, receiver: TUIGiftUser)
    func giftPlayView(_ giftPlayView: GiftPlayView, onPlayGiftAnimation gift: TUIGift)
}

let gLikeMaxAnimationCount: Int = 10
let Screen_Width = UIScreen.main.bounds.size.width
let Screen_Height = UIScreen.main.bounds.size.height
let Bottom_SafeHeight = WindowUtils.bottomSafeHeight

class GiftPlayView: UIView {
    weak var delegate: GiftPlayViewDelegate?
    
    private let roomId: String
    private let manager: GiftManager
    private var currentLikeAnimationCount: Int = 0
    private var likeCount = 0
    private var giftCacheKey = ""
    private let animationView: AnimationViewWrapper = AnimationViewWrapper()
    
    private lazy var normalAnimationManager: TUIGiftAnimationManager = {
        let manager = TUIGiftAnimationManager(simulcastCount: 99)
        manager.dequeueClosure = { [weak self] giftData in
            guard let self = self else { return }
            self.showNormalAnimation(giftData)
        }
        return manager
    }()
    
    private lazy var advancedAnimationManager: TUIGiftAnimationManager = {
        let manager = TUIGiftAnimationManager(simulcastCount: 1)
        manager.dequeueClosure = { [weak self] giftData in
            guard let self = self else { return }
            self.giftCacheKey = giftData.gift.giftId
            self.showAdvancedAnimation(giftData: giftData)
        }
        return manager
    }()
    
    private let likeColors: [UIColor] = [.red, .purple, .orange,
                                         .yellow, .green, .blue,
                                         .gray, .cyan, .brown,]
    
    init(roomId: String) {
        self.roomId = roomId
        self.manager = GiftManager(roomId: roomId)
        super.init(frame: .zero)
        isUserInteractionEnabled = false
        addObserver()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }
    
    private func clearData() {
        normalAnimationManager.clearData()
        advancedAnimationManager.clearData()
    }
    
    deinit {
        removeObserver()
    }
    
    private func addObserver() {
        manager.delegate = self
        
        TUIGiftStore.shared.giftDataMap.addObserver(self) { [weak self] giftDataMap, _ in
            guard let self = self, let giftData = giftDataMap[self.roomId] else { return }
            self.delegate?.giftPlayView(self,
                                        onReceiveGift: giftData.gift,
                                        giftCount: giftData.giftCount,
                                        sender: giftData.sender,
                                        receiver: giftData.receiver)
            playGift(giftData)
        }
        TUIGiftStore.shared.likeDataMap.addObserver(self) { [weak self] likeDataMap, _ in
            guard let self = self, let likeData = likeDataMap[self.roomId]  else { return }
            self.playLikeModel(sender: likeData.sender)
        }
    }
    
    private func playGift(_ gift: TUIGiftData) {
        if gift.isAdvanced {
            advancedAnimationManager.enqueue(giftData: gift)
        } else {
            normalAnimationManager.enqueue(giftData: gift)
        }
    }
    
    private func removeObserver() {
        TUIGiftStore.shared.giftDataMap.removeObserver(self)
        TUIGiftStore.shared.likeDataMap.removeObserver(self)
    }
}

fileprivate extension TUIGiftData {
    var isAdvanced: Bool {
        gift.animationUrl.count > 0
    }
}

// MARK: interface

extension GiftPlayView {
    func playGiftAnimation(playUrl: String) {
        animationView.playAnimation(playUrl: playUrl) { [weak self] code in
            guard let self = self else { return }
            if code != 0 {
                makeToast(.playFailedText)
            }
            self.advancedAnimationManager.finishPlay()
        }
    }
    
    func getLikeCount() -> Int {
        return likeCount
    }
}

// MARK: Layout

extension GiftPlayView {
    private func constructViewHierarchy() {
        addSubview(animationView)
    }
    
    private func activateConstraints() {
        animationView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
}


// MARK: Normal Animation

extension GiftPlayView {
    private func showNormalAnimation(_ giftData: TUIGiftData) {
        DataReporter.reportEventData(eventKey: getReportKey())
        let beginY = mm_h * 0.3
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
            self.normalAnimationManager.finishPlay()
        }
    }
}

// MARK: Advanced Animation

extension GiftPlayView {
    private func showAdvancedAnimation(giftData: TUIGiftData) {
        delegate?.giftPlayView(self, onPlayGiftAnimation: giftData.gift)
    }
}

// MARK: Like Animation

private extension GiftPlayView {
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

// MARK: TUIGiftPresenterDelegate

extension GiftPlayView: GiftManagerDelegate {
    func onGiftDidSend(_ model: TUIGift, sender: TUIGiftUser, receiver: TUIGiftUser, giftCount: Int, isSuccess: Bool, message: String) {
    }
    
    func onLikeDidSend(sender: TUIGiftUser, isSuccess: Bool, message: String) {
        
    }
    
    func onReceiveGift(_ model: TUIGift, giftCount: Int, sender: TUIGiftUser, receiver: TUIGiftUser) {
        playGift(TUIGiftData(gift: model, giftCount: giftCount, sender: sender, receiver: receiver))
        delegate?.giftPlayView(self, onReceiveGift: model, giftCount: giftCount, sender: sender, receiver: receiver)
    }
    
    func onReceiveLike(sender: TUIGiftUser) {
        playLikeModel(sender: sender)
        likeCount += 1
    }
}


// MARK: DataReport

extension GiftPlayView {
    private func getReportKey() -> Int {
        let isSupportEffectPlayer = isSupportEffectPlayer()
        var key = Constants.DataReport.kDataReportLiveGiftSVGAPlayCount
        if DataReporter.componentType == .liveRoom {
            key = isSupportEffectPlayer ? Constants.DataReport.kDataReportLiveGiftEffectPlayCount :
            Constants.DataReport.kDataReportLiveGiftSVGAPlayCount
        } else if DataReporter.componentType == .voiceRoom {
            key = isSupportEffectPlayer ? Constants.DataReport.kDataReportVoiceGiftEffectPlayCount :
            Constants.DataReport.kDataReportVoiceGiftSVGAPlayCount
        }
        return key
    }
    
    private func isSupportEffectPlayer() -> Bool {
        let service = TUICore.getService("TUIEffectPlayerService")
        return service != nil
    }
}

private extension String {
    static var playFailedText = localized("live.error.failed")
}
