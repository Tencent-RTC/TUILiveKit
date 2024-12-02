//
//  LSMenuContainerView.swift
//  Pods
//
//  Created by jeremiawang on 2024/11/18.
//

import UIKit
import SnapKit

class LSMenuContainerView: UIView {
    
    var blackAreaClickClosure: (()->Void)?
    private var isViewReady: Bool = false
    
    private let contentView: UIView
    
    private let container: UIView = {
        let view = UIView(frame: .zero)
        view.layer.cornerRadius = 16
        view.layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
        view.backgroundColor = .clear
        return view
    }()
    
    private let safeBottomView: UIView = {
        let view = UIView()
        return view
    }()
    
    init(contentView: UIView, safeBottomViewBackgroundColor: UIColor = .g2) {
        self.contentView = contentView
        self.safeBottomView.backgroundColor = safeBottomViewBackgroundColor
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
        container.addSubview(safeBottomView)
    }
    
    private func activeViewConstraint() {
        container.snp.makeConstraints { make in
            make.leading.trailing.bottom.equalToSuperview()
        }
        contentView.snp.remakeConstraints { make in
            make.top.leading.trailing.equalToSuperview()
            make.bottom.equalTo(safeAreaLayoutGuide.snp.bottom)
        }
        safeBottomView.snp.makeConstraints { make in
            make.leading.trailing.bottom.equalToSuperview()
            make.top.equalTo(safeAreaLayoutGuide.snp.bottom)
        }
    }
}
