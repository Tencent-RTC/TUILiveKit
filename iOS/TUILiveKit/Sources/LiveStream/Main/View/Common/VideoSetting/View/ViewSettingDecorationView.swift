//
//  ViewSettingDecorationView.swift
//  TUILiveKit
//
//  Created by jack on 2024/11/25.
//

import Foundation

class ViewSettingDecorationView: UICollectionReusableView {
    
    static let ReusableID: String = "ViewSettingDecorationView"
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        
        backgroundColor = UIColor.g3.withAlphaComponent(0.3)
        layer.cornerRadius = 12
        layer.masksToBounds = true
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
}

class ViewSettingTitleHeaderView: UICollectionReusableView {
    
    static let ReusableID: String = "ViewSettingTitleHeaderView"
    
    lazy var titleLabel: UILabel = {
        let label = UILabel()
        label.font = .customFont(ofSize: 14.0, weight: .medium)
        label.textColor = .g7
        label.textAlignment = .center
        return label
    }()
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        backgroundColor = .clear
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady: Bool = false
    override func layoutSubviews() {
        super.layoutSubviews()
        guard !isViewReady else { return }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
    }
    
}

extension ViewSettingTitleHeaderView {
    
    private func constructViewHierarchy() {
        addSubview(titleLabel)
    }
    
    private func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.leading.equalTo(20)
            make.centerY.equalToSuperview()
        }
    }
}

