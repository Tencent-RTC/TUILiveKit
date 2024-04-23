//
//  VoiceRoomRootView.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/4.
//

import UIKit

import SnapKit
import Combine

class BottomMenuView: UIView {
    private var isViewReady: Bool = false
    private var cancellables = Set<AnyCancellable>()
    private let buttonSliceIndex: Int = 1
    
    private let maxMenuButtonNumber = 4
    private let buttonWidth: CGFloat = 36.0
    private let buttonSpacing: CGFloat = 30.0
    
    @Injected var store: VoiceRoomStoreProvider
    lazy var menus = self.store.select(ViewSelectors.getBottomMenuButtons)
    
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
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activeViewConstraint()
        bindInteraction()
        setupStyle()
        isViewReady = true
    }
    
    private func constructViewHierarchy() {
        addSubview(stackView)
    }
    
    private func activeViewConstraint() {
        stackView.snp.makeConstraints { make in
            let maxWidth = buttonWidth * CGFloat(maxMenuButtonNumber) + buttonSpacing * CGFloat(maxMenuButtonNumber - 1)
            make.top.bottom.equalToSuperview()
            make.trailing.equalTo(-16)
            make.width.lessThanOrEqualTo(maxWidth)
            make.leading.equalToSuperview()
        }
    }
    
    private func bindInteraction() {
        menus
            .receive(on: RunLoop.main)
            .filter { !$0.isEmpty }
            .sink { [weak self] menus in
                guard let self = self else { return }
                self.updateButtons(menus: menus)
            }
            .store(in: &cancellables)
    }
    
    private func setupStyle() {
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
                    make.height.width.equalTo(36)
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
        item.bindStateClosure?(button, &cancellables)
        return button
    }
}

class MenuButton: UIButton {
    lazy var redDot: UIView = {
        let redDot = UIView()
        redDot.backgroundColor = .red
        redDot.layer.cornerRadius = 5
        redDot.layer.masksToBounds = true
        redDot.isHidden = true
        return redDot
    }()
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        addSubview(redDot)
        
        redDot.snp.makeConstraints { make in
            make.centerY.equalTo(snp.top)
            make.centerX.equalTo(snp.right)
            make.size.equalTo(CGSize(width: 10, height: 10))
        }
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
}

extension BottomMenuView {
    @objc
    func menuTapAction(sender: MenuButton) {
        let index = sender.tag - 1_000
        let menus = store.selectCurrent(ViewSelectors.getBottomMenuButtons)
        let bottomMenu = menus[index]
        bottomMenu.tapAction?(sender)
    }
}
