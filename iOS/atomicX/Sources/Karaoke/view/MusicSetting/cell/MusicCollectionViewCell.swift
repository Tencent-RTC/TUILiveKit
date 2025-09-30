//
//  CollectionViewCell.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/26.
//

import Foundation
import UIKit
import RTCCommon
import SnapKit

class MusicButtonCollectionCell: UICollectionViewCell {
    static let identifier = "MusicButtonCollectionCell"
    var item: MusicButtonItem?

    override var isSelected: Bool {
        didSet {
            updateBorder()
        }
    }
    
    let containerView: UIView = {
        let view = UIView()
        view.backgroundColor = .g3.withAlphaComponent(0.3)
        view.layer.cornerRadius = 10
        view.layer.masksToBounds = true
        return view
    }()
    
    private lazy var imageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.layer.cornerRadius = 8.scale375()
        imageView.layer.masksToBounds = true
        self.contentView.addSubview(imageView)
        return imageView
    }()
    
    private lazy var titleLabel: UILabel = {
        let view = UILabel()
        view.font = .customFont(ofSize: 12)
        view.textColor = .flowKitWhite
        view.textAlignment = .center
        self.contentView.addSubview(view)
        return view
    }()
    
    private var isViewReady = false
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        contentView.backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }
    
    private func constructViewHierarchy() {
        contentView.addSubview(containerView)
        containerView.addSubview(imageView)
        contentView.addSubview(titleLabel)
    }
    
    private func activateConstraints() {
        containerView.snp.makeConstraints { make in
            make.top.leading.equalToSuperview()
            make.width.height.equalTo(56.scale375())
        }
        
        imageView.snp.makeConstraints { make in
            make.centerX.centerY.equalToSuperview()
            make.width.height.equalTo(30.scale375())
        }
        
        titleLabel.snp.makeConstraints { make in
            make.top.equalTo(containerView.snp.bottom).offset(6.scale375())
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(18.scale375())
        }
    }
    
    private func updateBorder() {
        containerView.layer.borderColor = isSelected ? UIColor.b1.cgColor : UIColor.transparent.cgColor
        containerView.layer.borderWidth = isSelected ? 2 : 0
    }
    
    public func update(item: MusicSettingItem) {
        guard let buttonItem = item as? MusicButtonItem else { return }
        imageView.image = buttonItem.icon
        titleLabel.text = buttonItem.buttonTitle
    }
}

class MusicCollectionViewCell: UITableViewCell {
    public static let identifier = "MusicCollectionViewCell"

    private var items: [MusicSettingItem] = []
    private(set) var selectedItem: Int = 0
    
    private let configCollectionView: UICollectionView = {
        let layout = UICollectionViewFlowLayout()
        layout.scrollDirection = .horizontal
        layout.itemSize = CGSize(width: 56, height: 80)
        layout.minimumLineSpacing = 12.0
        let view = UICollectionView(frame: .zero, collectionViewLayout: layout)
        view.showsVerticalScrollIndicator = false
        view.showsHorizontalScrollIndicator = false
        view.backgroundColor = .clear
        view.register(MusicButtonCollectionCell.self, forCellWithReuseIdentifier: MusicButtonCollectionCell.identifier)
        return view
    }()
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        setupStyle()
        isViewReady = true
    }
    
    private func constructViewHierarchy() {
        contentView.addSubview(configCollectionView)
    }
    
    private func activateConstraints() {
        configCollectionView.snp.makeConstraints { make in
            make.trailing.equalToSuperview()
            make.leading.equalToSuperview()
            make.bottom.equalToSuperview()
            make.top.equalToSuperview()
        }
    }
    
    private func bindInteraction() {
        configCollectionView.dataSource = self
        configCollectionView.delegate = self
    }
    
    private func setupStyle() {
        backgroundColor = .clear
        selectionStyle = .none
    }
    
    func update(items: [MusicSettingItem]) {
        self.items = items
        configCollectionView.reloadData()
    }
}

extension MusicCollectionViewCell: UICollectionViewDelegate {
    public func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        guard let item = items[indexPath.item] as? MusicButtonItem else { return }
        item(payload: ())
    }
    
    public func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 1
    }
}

extension MusicCollectionViewCell: UICollectionViewDataSource {
    public func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return items.count
    }
    
    public func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: MusicButtonCollectionCell.identifier, for: indexPath)
        if let buttonCell = cell as? MusicButtonCollectionCell {
            let item = items[indexPath.item]
            buttonCell.update(item: item)
            if let buttonItem = item as? MusicButtonItem, buttonItem.isSelected {
                collectionView.selectItem(at: indexPath, animated: false, scrollPosition: .left)
            }
        }
        return cell
    }
}
