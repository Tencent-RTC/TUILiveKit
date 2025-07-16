//
//  FeatureCollectionView.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/11/21.
//

import Foundation
import UIKit
import RTCCommon

enum LiveKitClickEvent {
    case `default`
}

class FeatureCollectionViewDesignConfig {
    var scrollDirection: UICollectionView.ScrollDirection = .horizontal
    var itemSize:CGSize = CGSize(width: 56.scale375(), height: 80.scale375())
    var itemSpacing: CGFloat = 12.scale375()
    var hasHeader: Bool = true
}

class FeatureCollectionViewItem {
    init(title: String? = nil, image: UIImage? = nil, isSelected: Bool = false, action: Any) {
        normalTitle = title
        normalImage = image
        actionType = action
        self.isSelected = isSelected
    }

    var normalTitle: String?
    var normalImage: UIImage?
    var actionType: Any?
    var isSelected: Bool
}

class FeatureCollectionViewModel {
    var items: [FeatureCollectionViewItem] = []
}

class FeatureCollectionView: UIView {
    private let headerReuseIdentifier = "SectionHeader"
    private var model: FeatureCollectionViewModel
    private var headerTitle: String
    private var config: FeatureCollectionViewDesignConfig
    private var currentSelectedCellIndex: IndexPath?
    
    let clickEventCallBack: Observable<Any> = Observable(LiveKitClickEvent.default)
    init(headerTitle: String = "",
         model: FeatureCollectionViewModel,
         designConfig config: FeatureCollectionViewDesignConfig = FeatureCollectionViewDesignConfig()) {
        self.headerTitle = headerTitle
        self.model = model
        self.config = config
        super.init(frame: .zero)
        setView()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private lazy var titleLabel: UILabel = {
        let view = UILabel()
        view.text = self.headerTitle
        view.textColor = .g7
        view.font = .customFont(ofSize: 14)
        view.textAlignment = .left
        return view
    }()

    private lazy var collectionView: UICollectionView = {
        let layout = UICollectionViewFlowLayout()
        layout.scrollDirection = config.scrollDirection
        layout.itemSize = config.itemSize
        layout.minimumLineSpacing = config.itemSpacing
        let collectionView = UICollectionView(frame: .zero, collectionViewLayout: layout)
        collectionView.showsVerticalScrollIndicator = false
        collectionView.showsHorizontalScrollIndicator = false
        collectionView.backgroundColor = .clear
        collectionView.isUserInteractionEnabled = true
        collectionView.contentMode = .scaleToFill
        collectionView.dataSource = self
        collectionView.delegate = self
        collectionView.register(FeatureCell.self, forCellWithReuseIdentifier: FeatureCell.cellReuseIdentifier)
        return collectionView
    }()

    func setView() {
        if config.hasHeader { addSubview(titleLabel) }
        addSubview(collectionView)
        if config.hasHeader {
            titleLabel.snp.makeConstraints { make in
                make.top.leading.equalToSuperview()
                make.width.equalToSuperview()
                make.height.equalTo(20.scale375Height())
            }
        }
        collectionView.snp.makeConstraints { make in
            if config.hasHeader {
                make.top.equalTo(titleLabel.snp.bottom)
            } else {
                make.top.equalToSuperview()
            }
            make.leading.bottom.equalToSuperview()
            make.width.equalToSuperview()
        }
    }
}

extension FeatureCollectionView: UICollectionViewDelegate {
    func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        guard let actionType =  model.items[indexPath.row].actionType else{ return}
        clickEventCallBack.value = actionType
        handleCellSelectedState(indexPath)
    }
    
    func handleCellSelectedState(_ indexPath: IndexPath) {
        let preSelectedCellIndex = currentSelectedCellIndex
        currentSelectedCellIndex = indexPath
        if let index = preSelectedCellIndex,
           let cell = collectionView.cellForItem(at: index) as? FeatureCell {
            cell.isSelectedCell = false
        }
        if let index = currentSelectedCellIndex, let cell = collectionView.cellForItem(at: index) as? FeatureCell {
            cell.isSelectedCell = true
        }
    }
}

extension FeatureCollectionView: UICollectionViewDataSource {
    func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 1
    }

    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return model.items.count
    }

    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: FeatureCell.cellReuseIdentifier, for: indexPath) as! FeatureCell
        cell.titleImageView.image = model.items[indexPath.row].normalImage
        cell.titleLabel.text = model.items[indexPath.row].normalTitle
        cell.isSelectedCell = model.items[indexPath.row].isSelected
        if cell.isSelectedCell {
            currentSelectedCellIndex = indexPath
        }
        return cell
    }
}

class FeatureCell: UICollectionViewCell {
    var isSelectedCell: Bool = false {
        didSet {
            updateBorder()
        }
    }

    let imageBgView: UIView = {
        let view = UIView()
        view.backgroundColor = .g3.withAlphaComponent(0.3)
        view.layer.cornerRadius = 10
        view.layer.masksToBounds = true
        return view
    }()

    lazy var titleImageView: UIImageView = {
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
        setView()
        isViewReady = true
    }

    func setView() {
        contentView.addSubview(imageBgView)
        imageBgView.addSubview(titleImageView)
        contentView.addSubview(titleLabel)

        imageBgView.snp.makeConstraints { make in
            make.top.leading.equalToSuperview()
            make.width.height.equalTo(56.scale375())
        }

        titleImageView.snp.makeConstraints { make in
            make.centerX.centerY.equalToSuperview()
            make.width.height.equalTo(30.scale375())
        }

        titleLabel.snp.makeConstraints { make in
            make.top.equalTo(imageBgView.snp.bottom).offset(6.scale375Height())
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(18.scale375Height())
        }
    }

    func updateBorder() {
        imageBgView.layer.borderColor = isSelectedCell ? UIColor.b1.cgColor : UIColor.transparent.cgColor
        imageBgView.layer.borderWidth = isSelectedCell ? 2 : 0
    }
}
