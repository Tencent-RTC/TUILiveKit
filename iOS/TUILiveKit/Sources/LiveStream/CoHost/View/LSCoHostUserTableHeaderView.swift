//
//  LSCoHostUserTableHeaderView.swift
//  TUILiveKit
//
//  Created by jack on 2024/8/8.
//

import Foundation

class LSCoHostUserTableHeaderView: UITableViewHeaderFooterView {
    static let identifier = "LSCoHostUserTableHeaderView"
    lazy var titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .g7
        label.font = .customFont(ofSize: 14, weight: .medium)
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
    
    
    func constructViewHierarchy() {
        contentView.addSubview(titleLabel)
    }
    
    func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.leading.equalToSuperview().offset(24.scale375())
        }
    }
    
    func bindInteraction() {}
}
