//
//  LSSettingPanel.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/20.
//

import Combine
import Foundation
import RTCCommon

class LSSettingPanel: UIView {
    private let settingPanelModel: LSFeatureClickPanelModel
    
    private let titleLabel: UILabel = {
        let view = UILabel()
        view.text = .settingTitleText
        view.textColor = .textPrimaryColor
        view.font = .customFont(ofSize: 16, weight: .medium)
        view.textAlignment = .center
        return view
    }()

    private lazy var collectionView: UICollectionView = {
        let layout = UICollectionViewFlowLayout()
        layout.scrollDirection = .vertical
        let view = UICollectionView(frame: .zero, collectionViewLayout: layout)
        view.backgroundColor = .clear
        view.dataSource = self
        view.delegate = self
        view.showsVerticalScrollIndicator = false
        view.showsHorizontalScrollIndicator = false
        view.contentInset = UIEdgeInsets(top: 0, left: 24.scale375(), bottom: 0, right: 23.scale375())
        view.register(LSSettingPanelCell.self, forCellWithReuseIdentifier: LSSettingPanelCell.CellId)
        return view
    }()

    init(settingPanelModel: LSFeatureClickPanelModel) {
        self.settingPanelModel = settingPanelModel
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()

    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        setupView()
        isViewReady = true
    }

    private func setupView() {
        backgroundColor = .bgOperateColor
        layer.cornerRadius = 20
        layer.masksToBounds = true
    }
}

// MARK: Layout

private extension LSSettingPanel {
    func constructViewHierarchy() {
        addSubview(titleLabel)
        addSubview(collectionView)
    }

    func activateConstraints() {
        snp.remakeConstraints { make in
            if isPortrait {
                make.height.equalTo(289.scale375Height())
            } else {
                make.width.equalTo(375.scale375())
            }
            make.edges.equalToSuperview()
        }

        titleLabel.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(20.scale375Height())
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(24.scale375Height())
        }

        collectionView.snp.makeConstraints { make in
            make.top.equalTo(titleLabel.snp.bottom).offset(20.scale375Height())
            make.bottom.equalToSuperview()
            make.leading.trailing.equalToSuperview()
        }
    }
}

// MARK: - UICollectionViewDataSource
extension LSSettingPanel: UICollectionViewDataSource {

    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return settingPanelModel.items.count
    }
    
    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: LSSettingPanelCell.CellId, for: indexPath) as! LSSettingPanelCell
        cell.updateUI(settingPanelModel.items[indexPath.item])
        return cell
    }
    
}

// MARK: - UICollectionViewDelegateFlowLayout
extension LSSettingPanel: UICollectionViewDelegateFlowLayout {
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, sizeForItemAt indexPath: IndexPath) -> CGSize {
        return settingPanelModel.itemSize
    }
    
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, minimumLineSpacingForSectionAt section: Int) -> CGFloat {
        return settingPanelModel.itemDiff
    }
    
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, minimumInteritemSpacingForSectionAt section: Int) -> CGFloat {
        return settingPanelModel.itemDiff
    }
}

class LSSettingPanelCell: UICollectionViewCell {

    static let CellId: String = "LSSettingPanelCell"
    private var itemButton: LSFeatureItemButton?
    
    func constructViewHierarchy() {
        guard let btn = itemButton else {
            return
        }
        contentView.addSubview(btn)
        btn.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    func updateUI(_ item: LSFeatureItem) {
        if let btn = itemButton {
            btn.removeFromSuperview()
        }
        itemButton = LSFeatureItemButton(item: item)
        itemButton?.addTarget(self, action: #selector(itemClick(_:)), for: .touchUpInside)
        constructViewHierarchy()
    }

    @objc
    private func itemClick(_ sender: LSFeatureItemButton) {
        sender.item.actionClosure?(sender)
    }
}


private extension String {
    static let settingTitleText: String = internalLocalized("More Features")
}
