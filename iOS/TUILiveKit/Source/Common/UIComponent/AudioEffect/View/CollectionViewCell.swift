//
//  CollectionViewCell.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/26.
//

import Foundation

import UIKit

class ButtonCollectionCell: UICollectionViewCell {
    static let identifier = "ButtonCollectionCell"
    var item: ButtonItem?
    
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
    
    lazy var imageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.layer.cornerRadius = 8.scale375()
        imageView.layer.masksToBounds = true
        self.contentView.addSubview(imageView)
        return imageView
    }()
    
    lazy var titleLabel: UILabel = {
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
    
    func constructViewHierarchy() {
        contentView.addSubview(containerView)
        containerView.addSubview(imageView)
        contentView.addSubview(titleLabel)
    }
    
    func activateConstraints() {
        containerView.snp.makeConstraints { make in
            make.top.leading.equalToSuperview()
            make.width.height.equalTo(56.0)
        }
        
        imageView.snp.makeConstraints { make in
            make.centerX.centerY.equalToSuperview()
            make.width.height.equalTo(30.0)
        }
        
        titleLabel.snp.makeConstraints { make in
            make.top.equalTo(containerView.snp.bottom).offset(6.0)
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(18.0)
        }
    }
    
    func updateBorder() {
        containerView.layer.borderColor = isSelected ? UIColor.b1.cgColor : UIColor.transparent.cgColor
        containerView.layer.borderWidth = isSelected ? 2 : 0
    }
    
    func update(item: SettingItem) {
        guard let buttonItem = item as? ButtonItem else { return }
        imageView.image = buttonItem.icon
        titleLabel.text = buttonItem.buttonTitle
    }
}

class CollectionViewCell: UITableViewCell {
    static let identifier = "CollectionViewCell"
    
    private var items: [SettingItem] = []
    private(set) var selectedItem: Int = 0
    
    let configCollectionView: UICollectionView = {
        let layout = UICollectionViewFlowLayout()
        layout.scrollDirection = .horizontal
        layout.itemSize = CGSize(width: 56, height: 80)
        layout.minimumLineSpacing = 12.0
        let view = UICollectionView(frame: .zero, collectionViewLayout: layout)
        view.showsVerticalScrollIndicator = false
        view.showsHorizontalScrollIndicator = false
        view.backgroundColor = .clear
        view.register(ButtonCollectionCell.self, forCellWithReuseIdentifier: ButtonCollectionCell.identifier)
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
    
    func setupStyle() {
        backgroundColor = .clear
        selectionStyle = .none
    }
    
    func update(items: [SettingItem]) {
        self.items = items
        configCollectionView.reloadData()
    }
}

extension CollectionViewCell: UICollectionViewDelegate {
    func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        guard let item = items[indexPath.item] as? ButtonItem else { return }
        item(payload: ())
    }
    
    func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 1
    }
}

extension CollectionViewCell: UICollectionViewDataSource {
    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return items.count
    }
    
    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: ButtonCollectionCell.identifier, for: indexPath)
        if let buttonCell = cell as? ButtonCollectionCell {
            let item = items[indexPath.item]
            buttonCell.update(item: item)
            if let buttonItem = item as? ButtonItem, buttonItem.isSelected {
                collectionView.selectItem(at: indexPath, animated: false, scrollPosition: .left)
            }
        }
        return cell
    }
}
