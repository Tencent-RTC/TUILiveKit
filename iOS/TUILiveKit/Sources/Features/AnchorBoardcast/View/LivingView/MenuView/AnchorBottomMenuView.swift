//
//  AnchorBottomMenuView.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/18.
//

import UIKit
import RTCCommon
import SnapKit
import Combine
import AtomicXCore

protocol AnchorBottomMenuViewDelegate: NSObjectProtocol {
    func likeButtonClicked()
}

public enum AnchorBottomMenuFeature: CaseIterable {
    case coGuest
    case coHost
    case battle
    
    // Settings
    case beauty
    case soundEffect
}

class AnchorBottomMenuView: RTCBaseView {
    var cancellableSet = Set<AnyCancellable>()
    
    private let manager: AnchorManager
    private let routerManager: AnchorRouterManager
    private let coreView: LiveCoreView
    private let buttonSliceIndex: Int = 1
    
    private let maxMenuButtonNumber = 5
    private let buttonWidth: CGFloat = 36.0
    private let buttonSpacing: CGFloat = 6.0
    
    private var anchorFeatures: [AnchorBottomMenuFeature] = AnchorBottomMenuFeature.allCases
    
    private lazy var creator = AnchorMenuDataCreator(coreView: coreView, manager: manager, routerManager: routerManager)
    private var menus = [AnchorButtonMenuInfo]()
    
    let stackView: UIStackView = {
        let view = UIStackView()
        view.axis = .horizontal
        view.alignment = .center
        view.distribution = .fillProportionally
        return view
    }()
    
    var buttons: [UIButton] = []
    
    init(manager: AnchorManager, routerManager: AnchorRouterManager, coreView: LiveCoreView) {
        self.manager = manager
        self.routerManager = routerManager
        self.coreView = coreView
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    override func constructViewHierarchy() {
        addSubview(stackView)
    }
    
    override func activateConstraints() {
        stackView.snp.makeConstraints { make in
            let maxWidth = buttonWidth * CGFloat(maxMenuButtonNumber) + buttonSpacing * CGFloat(maxMenuButtonNumber - 1)
            make.centerY.equalToSuperview()
            make.trailing.equalTo(-16)
            make.width.lessThanOrEqualTo(maxWidth)
            make.leading.equalToSuperview()
        }
    }
    
    override func setupViewStyle() {
        stackView.spacing = buttonSpacing
        setupButtons()
    }
    
    func disableFeature(_ feature: AnchorBottomMenuFeature, isDisable: Bool) {
        if isDisable {
            anchorFeatures.removeAll(where: { $0 == feature })
            setupButtons()
        } else {
            if !anchorFeatures.contains(where: { $0 == feature }) {
                anchorFeatures.append(feature)
                setupButtons()
            }
        }
    }
    
    private func setupButtons() {
        menus = creator.generateBottomMenuData(features: anchorFeatures)
        stackView.subviews.forEach { view in
            stackView.removeArrangedSubview(view)
            view.safeRemoveFromSuperview()
        }
        buttons = menus
            .enumerated().map { value -> AnchorMenuButton in
                let index = value.offset
                let item = value.element
                let button = self.createButtonFromMenuItem(index: index, item: item)
                stackView.addArrangedSubview(button)
                button.snp.makeConstraints { make in
                    make.width.equalTo(34.scale375())
                    make.height.equalTo(46.scale375())
                    make.centerY.equalToSuperview()
                }
                button.addTarget(self, action: #selector(menuTapAction(sender:)), for: .touchUpInside)
                return button
            }
    }
    
    private func createButtonFromMenuItem(index: Int, item: AnchorButtonMenuInfo) -> AnchorMenuButton {
        let button = AnchorMenuButton()
        button.setupItem(item)
        button.tag = index + 1_000
        item.bindStateClosure?(button, &cancellableSet)
        return button
    }
}

class AnchorMenuButton: UIButton {
    
    private var item: AnchorButtonMenuInfo?
    private var timer: Timer?
    private var currentImageIndex = 0
    
    let rotateAnimation: CABasicAnimation = {
        let animation = CABasicAnimation(keyPath: "transform.rotation.z")
        animation.fromValue = 0
        animation.toValue = Double.pi * 2
        animation.duration = 2
        animation.autoreverses = false
        animation.fillMode = .forwards
        animation.repeatCount = MAXFLOAT
        animation.isRemovedOnCompletion = false
        return animation
    }()
    
    let redDotContentView: UIView = {
        let view = UIView()
        view.backgroundColor = .redDotColor
        view.layer.cornerRadius = 10.scale375Height()
        view.layer.masksToBounds = true
        view.isHidden = true
        return view
    }()
    
    let redDotLabel: UILabel = {
        let redDot = UILabel()
        redDot.textColor = .white
        redDot.textAlignment = .center
        redDot.font = UIFont(name: "PingFangSC-Semibold", size: 12)
        return redDot
    }()
    
    init() {
        super.init(frame: .zero)
        addSubview(redDotContentView)
        redDotContentView.addSubview(redDotLabel)
        
        redDotContentView.snp.makeConstraints { make in
            make.centerY.equalTo(snp.top).offset(5.scale375Height())
            make.centerX.equalTo(snp.right).offset(-5.scale375Height())
            make.height.equalTo(20.scale375Height())
            make.width.greaterThanOrEqualTo(20.scale375Height())
        }
        
        redDotLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(4.scale375())
            make.trailing.equalToSuperview().offset(-4.scale375())
            make.centerY.equalToSuperview()
        }
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        timer?.invalidate()
    }
    
    override func layoutSubviews() {
        super.layoutSubviews()
        
        guard let imageView = imageView, let titleLabel = titleLabel else { return }
        let imageViewSize: CGFloat = 28.scale375()
        let imageViewX = (bounds.width - imageViewSize) / 2
        let titleLabelHeight: CGFloat = 16.scale375()
        imageView.frame = CGRect(
            x: imageViewX,
            y: 0,
            width: imageViewSize,
            height: imageViewSize
        )
        
        titleLabel.frame = CGRect(
            x: 0,
            y: imageView.frame.maxY + 2,
            width: bounds.width,
            height: titleLabelHeight
        )
    }
    
    func setupItem(_ item: AnchorButtonMenuInfo) {
        self.item = item
        setImage(internalImage(item.normalIcon), for: .normal)
        setImage(internalImage(item.selectIcon), for: .selected)
        setTitle(item.normalTitle, for: .normal)
        setTitle(item.selectTitle, for: .selected)
        titleLabel?.font = UIFont(name: "PingFangSC-Semibold", size: 10)
        titleLabel?.textAlignment = .center
        setTitleColor(.white, for: .normal)
    }
    
    func startAnimating() {
        guard let item = item, !item.animateIcon.isEmpty, timer == nil else { return }
        timer = Timer.scheduledTimer(timeInterval: 0.5, target: self, selector: #selector(switchImage), userInfo: nil, repeats: true)
    }
    
    func stopAnimating() {
        guard let item = item else { return }
        timer?.invalidate()
        timer = nil
        setImage(internalImage(item.normalIcon), for: .normal)
        currentImageIndex = 0
    }
    
    @objc func switchImage() {
        guard let item = item else { return }
        currentImageIndex = (currentImageIndex + 1) % item.animateIcon.count
        let nextImage = internalImage(item.animateIcon[currentImageIndex])
        self.setImage(nextImage, for: .normal)
    }
    
    func updateDotCount(count: Int) {
        if count == 0 {
            redDotContentView.isHidden = true
        } else {
            redDotContentView.isHidden = false
            redDotLabel.text = "\(count)"
        }
    }
    
    func startRotate() {
        layer.add(rotateAnimation, forKey: nil)
    }
    
    func endRotate() {
        layer.removeAllAnimations()
    }
}

extension AnchorBottomMenuView {
    @objc func menuTapAction(sender: AnchorMenuButton) {
        let index = sender.tag - 1_000
        let bottomMenu = menus[index]
        bottomMenu.tapAction?(sender)
    }
}
