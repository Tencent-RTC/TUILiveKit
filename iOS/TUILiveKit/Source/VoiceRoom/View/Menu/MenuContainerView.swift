//
//  MenuContainer.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/19.
//

import UIKit
import SnapKit

class MenuContainerView: UIView {
    
    var blackAreaClickClosure: (()->Void)?
    private var isViewReady: Bool = false
    
    private let contentView: UIView
    
    private let container: UIView = {
        let view = UIView(frame: .zero)
        view.layer.cornerRadius = 16
        view.layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
        view.backgroundColor = .g2
        return view
    }()
    
    init(contentView: UIView) {
        self.contentView = contentView
        super.init(frame: .zero)
        self.backgroundColor = UIColor.clear
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
        isViewReady = true
    }
    
    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        super.touchesBegan(touches, with: event)
        guard let touch = touches.first else { return }
        let point = touch.location(in: container)
        guard !container.layer.contains(point) else { return }
        blackAreaClickClosure?()
    }
    
    private func constructViewHierarchy() {
        addSubview(container)
        container.addSubview(contentView)
    }
    
    private func activeViewConstraint() {
        container.snp.makeConstraints { make in
            make.left.right.bottom.equalToSuperview()
        }
        contentView.snp.remakeConstraints { make in
            make.top.left.right.equalToSuperview()
            make.bottom.equalTo(safeAreaLayoutGuide.snp.bottom)
        }
    }
}
