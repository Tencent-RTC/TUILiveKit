//
//  LSSystemImageSelectionPanel.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/19.
//

import Foundation
import Combine
import RTCRoomEngine
import RTCCommon

class LSSystemImageSelectionPanel: UIView {
    
    var backButtonClickClosure: (()->Void)?
    private var cancellableSet = Set<AnyCancellable>()
    private let editInfo: EditInfo
    
    private var configs: [LSSystemImageModel]
    private var currentSelectModel: LSSystemImageModel?
    
    private var isPortrait: Bool = {
        return WindowUtils.isPortrait
    }()
    
    private lazy var backButton: UIButton = {
        let view = UIButton(type: .system)
        view.setBackgroundImage(internalImage("live_back_icon"), for: .normal)
        view.addTarget(self, action: #selector(backButtonClick), for: .touchUpInside)
        return view
    }()
    
    private let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.contentMode = .center
        label.font = .customFont(ofSize: 16,weight: .medium)
        label.textColor = .g7
        return label
    }()
    
    private lazy var collectionView: UICollectionView = {
        let flowLayout = UICollectionViewFlowLayout()
        flowLayout.itemSize = CGSize(width: 100.scale375(), height: 100.scale375())
        flowLayout.minimumLineSpacing = (UIScreen.main.bounds.width - flowLayout.itemSize.width * 3.0) / 4.0
        flowLayout.minimumInteritemSpacing = flowLayout.minimumLineSpacing
        
        let collection = UICollectionView(frame: .zero,
                                          collectionViewLayout: flowLayout)
        collection.contentInset = UIEdgeInsets(top: 0,
                                               left: flowLayout.minimumLineSpacing,
                                               bottom: 0,
                                               right: flowLayout.minimumLineSpacing)
        collection.register(LSSystemImageCell.self, forCellWithReuseIdentifier: LSSystemImageCell.cellReuseIdentifier)
        collection.isPagingEnabled = false
        collection.showsVerticalScrollIndicator = false
        collection.showsHorizontalScrollIndicator = false
        collection.backgroundColor = .clear
        collection.contentInsetAdjustmentBehavior = .never
        collection.dataSource = self
        collection.delegate = self
        return collection
    }()
    
    private lazy var confirmButton: UIButton = {
        let view = UIButton(type: .system)
        view.frame = CGRect(origin: .zero, size: CGSize(width: 200.scale375(), height: 52.scale375()))
        view.showsTouchWhenHighlighted = false
        view.titleLabel?.font = .customFont(ofSize: 16,weight: .medium)
        view.setTitleColor(.flowKitWhite, for:  .normal)
        view.addTarget(self, action: #selector(confirmButtonClick), for: .touchUpInside)
        view.layer.cornerRadius = view.mm_h*0.5
        view.layer.masksToBounds = true
        view.backgroundColor = .brandBlueColor
        return view
    }()
    
    init(configs:[LSSystemImageModel], editInfo: inout EditInfo) {
        self.configs = configs
        self.editInfo = editInfo
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        backgroundColor = .clear
        constructViewHierarchy()
        setupView()
        activateConstraints()
        defaultSelectItem()
    }
    
}

// MARK: Layout
extension LSSystemImageSelectionPanel {
    func constructViewHierarchy() {
        addSubview(backButton)
        addSubview(titleLabel)
        addSubview(collectionView)
        addSubview(confirmButton)
    }
    
    func activateConstraints() {
        snp.makeConstraints { make in
            if isPortrait {
                make.height.equalTo(718.scale375Height())
            } else {
                make.width.equalTo(375)
            }
            make.left.right.top.equalToSuperview()
        }
        
        backButton.snp.makeConstraints { make in
            make.leading.equalToSuperview().inset(20)
            make.top.equalToSuperview().inset(20)
            make.height.equalTo(24.scale375())
            make.width.equalTo(24.scale375())
        }
        
        titleLabel.snp.makeConstraints { make in
            make.centerY.equalTo(backButton)
            make.centerX.equalToSuperview()
            make.height.equalTo(24.scale375())
        }
        
        collectionView.snp.makeConstraints { make in
            make.leading.trailing.bottom.equalToSuperview()
            make.top.equalTo(backButton.snp.bottom).offset(32)
        }
        
        confirmButton.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.bottom.equalToSuperview().inset(34.scale375Height())
            make.height.equalTo(confirmButton.mm_h)
            make.width.equalTo(confirmButton.mm_w)
        }
    }
    
    private func setupView() {
        backgroundColor = .g2
        layer.cornerRadius = 16
        layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
        titleLabel.text = .coverTitleText
        confirmButton.setTitle(.coverConfirmText, for: .normal)
    }
    
    private func defaultSelectItem() {
        var imageUrlPath: String = ""
        imageUrlPath = editInfo.coverUrl
        if let index = configs.firstIndex(where: { imageUrlPath.contains($0.imagePath) }) {
            collectionView.selectItem(at: IndexPath(item: index, section: 0), animated: false, scrollPosition: .top)
        }
    }
}

// MARK: Action
extension LSSystemImageSelectionPanel {
    @objc
    func backButtonClick() {
        backButtonClickClosure?()
    }
    
    @objc
    func confirmButtonClick() {
        guard let newImageUrlString = currentSelectModel?.imageUrl?.absoluteString else {
            backButtonClickClosure?()
            return
        }
        editInfo.coverUrl = newImageUrlString
        backButtonClickClosure?()
    }
}


// MARK: - UICollectionViewDelegateFlowLayout
extension LSSystemImageSelectionPanel: UICollectionViewDelegateFlowLayout,
                                     UIScrollViewDelegate,
                                     UICollectionViewDataSource {
    
    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return configs.count
    }
    
    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(
            withReuseIdentifier: LSSystemImageCell.cellReuseIdentifier,
            for: indexPath)
        if indexPath.item >= configs.count { return cell }
        if let imageCell = cell as? LSSystemImageCell {
            let model = configs[indexPath.item]
            imageCell.model = model
        }
        return cell
    }
    
    func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        guard let cell = collectionView.cellForItem(at: indexPath) as? LSSystemImageCell  else { return }
        self.currentSelectModel = cell.model
    }
}

private extension String {
    static let coverTitleText = internalLocalized("Cover")
    static let coverConfirmText = internalLocalized("Set as cover")
}

