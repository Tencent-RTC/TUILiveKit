//
//  MainCollectionCell.swift
//  TUILiveKitApp
//
//  Created by jack on 2024/10/8.
//

import Foundation
import UIKit

class MainCollectionCell: UICollectionViewCell {
    
    static let CellID: String = "MainCollectionCell"

    private var gradientColors: [UIColor] = []
    private lazy var containerView:UIView = {
        let containerView = UIView()
        containerView.layer.cornerRadius = 6
        containerView.layer.masksToBounds = true
        containerView.backgroundColor = .white
        return containerView
    }()

    private lazy var titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = UIColor("262B32")
        label.textAlignment = .left
        label.adjustsFontSizeToFitWidth = true
        label.minimumScaleFactor = 0.5
        return label
    }()
    
    private lazy var iconImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.contentMode = .scaleAspectFit
        return imageView
    }()
    
    private lazy var descLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = UIFont(name: "PingFangSC-Regular", size: convertPixel(w: 12))
        label.textColor = UIColor("626E84")
        label.textAlignment = .left
        label.numberOfLines = 0
        label.adjustsFontSizeToFitWidth = true
        label.minimumScaleFactor = 0.8
        return label
    }()
    
    private let arrowImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.image = UIImage(named: "main_arrow")
        imageView.contentMode = .scaleAspectFit
        return imageView
    }()
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }
    
    override func draw(_ rect: CGRect) {
        super.draw(rect)
        let gradientLayer = containerView.gradient(colors: gradientColors)
        gradientLayer.startPoint = CGPoint(x: 0.5, y: 0.0)
        gradientLayer.endPoint = CGPoint(x: 0.5, y: 1.0)
    }
    
    private func constructViewHierarchy() {
        contentView.addSubview(containerView)
        containerView.addSubview(titleLabel)
        containerView.addSubview(iconImageView)
        containerView.addSubview(descLabel)
        containerView.addSubview(arrowImageView)
    }
    
    private func activateConstraints() {
        containerView.snp.makeConstraints { make in
            make.top.left.equalToSuperview().offset(convertPixel(h: 4))
            make.bottom.right.equalToSuperview().offset(convertPixel(h: -4))
        }
        iconImageView.snp.makeConstraints { make in
            make.top.left.equalToSuperview().offset(16)
            make.width.height.equalTo(24)
        }
        titleLabel.snp.makeConstraints { make in
            make.left.equalTo(iconImageView.snp.right).offset(convertPixel(w: 6))
            make.right.equalToSuperview().offset(convertPixel(w: -6))
            make.centerY.equalTo(iconImageView)
        }
        descLabel.snp.makeConstraints { make in
            make.left.equalToSuperview().offset(convertPixel(w: 14))
            make.right.equalToSuperview().offset(convertPixel(w: -14))
            make.top.equalTo(iconImageView.snp.bottom).offset(convertPixel(h: 12)).priority(.high)
            make.bottom.lessThanOrEqualToSuperview().offset(convertPixel(h: -14))
        }
        arrowImageView.snp.makeConstraints { make in
            make.centerY.equalTo(iconImageView)
            make.right.equalToSuperview().offset(-16)
            make.size.equalTo(CGSize(width: 16.0, height: 16.0))
        }
    }
    
}

extension MainCollectionCell {
    
    public func config(_ item: MainItemModel) {
        titleLabel.text = item.title
        descLabel.text = item.content
        iconImageView.image = UIImage(named: item.imageName)
    }
}
