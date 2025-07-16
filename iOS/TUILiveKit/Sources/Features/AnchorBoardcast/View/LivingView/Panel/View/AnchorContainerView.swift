//
//  AnchorAlertContainerView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/11/26.
//

import UIKit

class AnchorAlertContainerView: UIView {
    private var isViewReady: Bool = false
    private let contentView: UIView
    
    private let container: UIView = {
        let view = UIView(frame: .zero)
        view.layer.cornerRadius = 16
        view.layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
        view.backgroundColor = .clear
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
        showContentViewWithAnimation()
    }
    
    private func showContentViewWithAnimation() {
        contentView.alpha = 0
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            UIView.animate(withDuration: 0.15, delay: 0, options:.curveEaseOut, animations: { [weak self] in
                guard let self = self else { return }
                self.contentView.alpha = 1
            }, completion: { _ in
                self.isViewReady = true
            })
        }
    }
    
    private func constructViewHierarchy() {
        addSubview(container)
        container.addSubview(contentView)
    }
    
    private func activeViewConstraint() {
        container.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        contentView.snp.remakeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
}
