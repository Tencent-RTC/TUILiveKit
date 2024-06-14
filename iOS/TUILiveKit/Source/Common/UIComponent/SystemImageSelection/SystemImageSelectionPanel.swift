//
//  SystemImageSelectionPanel.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/9.
//

import Foundation
import Combine

class SystemImageSelectionPanel: UIView {
    var backButtonClickClosure: (()->Void)?
    @WeakLazyInjected private var store: LiveStore?
    private var cancellableSet = Set<AnyCancellable>()
    
    private var isPortrait: Bool = {
        return WindowUtils.isPortrait
    }()

    private var isViewReady: Bool = false
    override func layoutSubviews() {
        super.layoutSubviews()
        guard !isViewReady else { return }
        isViewReady = true
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
    }

    private var configs: [SystemImageModel]
    private var currentSelectModel: SystemImageModel?
    init(configs:[SystemImageModel]) {
        self.configs = configs
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private lazy var backButton: UIButton = {
        let view = UIButton(type: .system)
        view.setBackgroundImage(.liveBundleImage("live_back_icon"), for: .normal)
        view.addTarget(self, action: #selector(backButtonClick), for: .touchUpInside)
        return view
    }()

    private let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.contentMode = .center
        label.font = .customFont(ofSize: 16,weight: .medium)
        label.textColor = .g7
        label.text = .systemImageSelectionTitle
        label.sizeToFit()
        return label
    }()
    
    private lazy var collectionView: UICollectionView = {
        let collection = UICollectionView(frame: .zero,
                                          collectionViewLayout: SystemImageLayout(cellCount: configs.count))
        collection.register(SystemImageCell.self, forCellWithReuseIdentifier: SystemImageCell.cellReuseIdentifier)
        collection.isPagingEnabled = false
        collection.showsVerticalScrollIndicator = false
        collection.showsHorizontalScrollIndicator = false
        collection.isUserInteractionEnabled = true
        collection.contentMode = .scaleToFill
        collection.backgroundColor = .clear
        collection.isPrefetchingEnabled = true
        collection.contentInsetAdjustmentBehavior = .never
        collection.dataSource = self
        collection.delegate = self
        return collection
    }()


    private lazy var confirmButton: UIButton = {
        let view = UIButton(type: .system)
        view.frame = CGRect(origin: .zero, size: CGSize(width: 200.scale375(), height: 52.scale375()))
        view.showsTouchWhenHighlighted = false
        view.setTitle(.selectionConfirmTitle, for: .normal)
        view.titleLabel?.font = .customFont(ofSize: 16,weight: .medium)
        view.setTitleColor(.flowKitWhite, for:  .normal)
        view.addTarget(self, action: #selector(confirmButtonClick), for: .touchUpInside)
        view.layer.cornerRadius = view.mm_h*0.5
        view.layer.masksToBounds = true
        view.backgroundColor = .brandBlueColor
        return view
    }()
    
}

// MARK: Layout

extension SystemImageSelectionPanel {
    func constructViewHierarchy() {
        backgroundColor = .g2
        self.layer.cornerRadius = 16
        self.layer.masksToBounds = true
        addSubview(backButton)
        addSubview(titleLabel)
        addSubview(collectionView)
        addSubview(confirmButton)
    }

    func activateConstraints() {
        snp.remakeConstraints { make in
            if isPortrait {
                make.height.equalTo(718.scale375Height())
            } else {
                make.width.equalTo(375)
            }
            make.edges.equalToSuperview()
        }
        
        backButton.snp.remakeConstraints { make in
            make.leading.equalToSuperview().inset(20)
            make.top.equalToSuperview().inset(20)
            make.height.equalTo(24.scale375())
            make.width.equalTo(24.scale375())
        }

        titleLabel.snp.remakeConstraints { make in
            make.centerY.equalTo(backButton)
            make.centerX.equalToSuperview()
            make.height.equalTo(24.scale375())
            make.width.equalTo(titleLabel.mm_w)
        }
        
        collectionView.snp.remakeConstraints { make in
            make.leading.trailing.bottom.equalToSuperview()
            make.top.equalTo(backButton.snp.bottom).offset(32)
        }
        
        confirmButton.snp.remakeConstraints { make in
            make.centerX.equalToSuperview()
            make.bottom.equalToSuperview().inset(34.scale375Height())
            make.height.equalTo(confirmButton.mm_h)
            make.width.equalTo(confirmButton.mm_w)
        }
        
    }
}

// MARK: Action

extension SystemImageSelectionPanel {
    @objc func backButtonClick() {
        backButtonClickClosure?()
    }

    @objc func confirmButtonClick() {
        guard let currentCoverUrl = store?.selectCurrent(RoomSelectors.getRoomCoverUrl)?.path else { return }
        store?.dispatch(action: RoomActions.updateRoomCoverUrl(payload: currentSelectModel?.url ?? currentCoverUrl))
        backButtonClickClosure?()
    }
}


// MARK: - UICollectionViewDelegateFlowLayout

extension SystemImageSelectionPanel: UICollectionViewDelegateFlowLayout,
    UIScrollViewDelegate,
    UICollectionViewDataSource {
    func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 1
    }

    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return configs.count
    }

    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(
            withReuseIdentifier: SystemImageCell.cellReuseIdentifier,
            for: indexPath) as! SystemImageCell
        if indexPath.item >= configs.count {
            return cell
        }
        let model = configs[indexPath.item]
        cell.model = model
        if model.isSelected.value {
            self.currentSelectModel?.isSelected.value = false
            self.currentSelectModel = model
        }
        return cell
    }
    
    func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        guard let cell = collectionView.cellForItem(at: indexPath) as? SystemImageCell  else { return }
        self.currentSelectModel?.isSelected.value = false
        self.currentSelectModel = cell.model
        self.currentSelectModel?.isSelected.value = true
    }
}

private extension String {
    static var systemImageSelectionTitle: String {
        localized("live.anchor.system.image.selection.title")
    }
    
    static var selectionConfirmTitle: String {
        localized("live.anchor.system.image.selection.confirm")
    }
    
}

