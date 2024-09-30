//
//  VoiceRoomRootView.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/4.
//

import UIKit
import RTCCommon
import SnapKit
import Combine

class BottomMenuView: RTCBaseView {
    var cancellableSet = Set<AnyCancellable>()
    private let buttonSliceIndex: Int = 1
    
    private let maxMenuButtonNumber = 5
    private let buttonWidth: CGFloat = 36.0
    private let buttonSpacing: CGFloat = 6.0
    
    @Published var menus = [ButtonMenuInfo]()
    
    let stackView: UIStackView = {
        let view = UIStackView()
        view.axis = .horizontal
        view.alignment = .center
        view.distribution = .fillProportionally
        return view
    }()
    
    var buttons: [UIButton] = []
    
    override init(frame: CGRect) {
        super.init(frame: frame)
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
    
    override func bindInteraction() {
        $menus
            .receive(on: RunLoop.main)
            .filter { !$0.isEmpty }
            .sink { [weak self] menus in
                guard let self = self else { return }
                self.updateButtons(menus: menus)
            }
            .store(in: &cancellableSet)
    }
    
    override func setupViewStyle() {
        stackView.spacing = buttonSpacing
    }
    
    private func updateButtons(menus: [ButtonMenuInfo]) {
        stackView.subviews.forEach { view in
            stackView.removeArrangedSubview(view)
            view.removeFromSuperview()
        }
        buttons = menus
            .enumerated().map { value -> MenuButton in
                let index = value.offset
                let item = value.element
                let button = self.createButtonFromMenuItem(index: index, item: item)
                stackView.addArrangedSubview(button)
                button.snp.makeConstraints { make in
                    make.height.width.equalTo(32.scale375Height())
                }
                button.addTarget(self, action: #selector(menuTapAction(sender:)), for: .touchUpInside)
                return button
            }
    }
    
    private func createButtonFromMenuItem(index: Int, item: ButtonMenuInfo) -> MenuButton {
        let button = MenuButton(frame: .zero)
        button.setImage(.liveBundleImage(item.normalIcon), for: .normal)
        button.setImage(.liveBundleImage(item.selectIcon), for: .selected)
        button.setTitle(item.normalTitle, for: .normal)
        button.setTitle(item.selectTitle, for: .selected)
        button.tag = index + 1_000
        item.bindStateClosure?(button, &cancellableSet)
        return button
    }
}

class MenuButton: UIButton {
    
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
    
    override init(frame: CGRect) {
        super.init(frame: frame)
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

extension BottomMenuView {
    @objc
    func menuTapAction(sender: MenuButton) {
        let index = sender.tag - 1_000
        let bottomMenu = menus[index]
        bottomMenu.tapAction?(sender)
    }
}
