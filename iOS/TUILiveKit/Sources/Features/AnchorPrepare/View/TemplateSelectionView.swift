//
//  TemplateSelectionView.swift
//  TUILiveKit
//
//  Created by gg on 2025/7/25.
//

import SnapKit
import RTCCommon

class TemplateSelectionView: UIView {
    var onSelectPkMode: ((LiveTemplateMode) -> ())?
    var onSelectMode: ((LiveTemplateMode) -> ())?
    var onCloseClosure: (() -> ())?
    
    private let dataSource: [SelectionMode] = SelectionMode.allCases
    private let margin: CGFloat = 12
    
    private var currentPkMode: LiveTemplateMode = .verticalGridDynamic
    private var currentMode: LiveTemplateMode = .verticalGridDynamic
    
    init(defaultMode: LiveTemplateMode, defaultPkMode: LiveTemplateMode, frame: CGRect) {
        self.currentPkMode = defaultPkMode
        self.currentMode = defaultMode
        
        super.init(frame: frame)
        
        backgroundColor = UIColor(hex: "#1F2024")
        layer.cornerRadius = 16
        clipsToBounds = true
        
        initTitleView()
        
        addSubview(collectionView)
        collectionView.snp.makeConstraints { make in
            make.top.equalTo(titleView.snp.bottom)
            make.leading.trailing.bottom.equalToSuperview()
            make.height.equalTo(getCollectionViewHeight())
        }
    }
    
    private lazy var titleView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .clear
        return view
    }()
    
    private func initTitleView() {
        addSubview(titleView)
        titleView.snp.makeConstraints { make in
            make.top.leading.trailing.equalToSuperview()
        }
        
        let backBtn = UIButton(type: .custom)
        backBtn.setImage(internalImage("live_back_icon"), for: .normal)
        backBtn.addTarget(self, action: #selector(backButtonClick), for: .touchUpInside)
        titleView.addSubview(backBtn)
        backBtn.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(20)
            make.bottom.equalToSuperview().offset(-20)
            make.leading.equalToSuperview().offset(24)
            make.size.equalTo(CGSize(width: 24, height: 24))
        }
        
        let titleLabel = UILabel(frame: .zero)
        titleLabel.font = .customFont(ofSize: 16, weight: .medium)
        titleLabel.textColor = .white.withAlphaComponent(0.9)
        titleLabel.text = .titleText
        titleView.addSubview(titleLabel)
        titleLabel.snp.makeConstraints { make in
            make.center.equalToSuperview()
        }
    }
    
    @objc private func backButtonClick(sender: UIButton) {
        onCloseClosure?()
    }
    
    private func getCollectionViewHeight() -> CGFloat {
        let seatRow: CGFloat = 2
        let seatHeight = CGFloat(56) * seatRow + margin * (2 + seatRow - 1)
        let pkHeight = CGFloat(56) + margin * 2
        return 12 + pkHeight + 25 + 20 + 12 + seatHeight
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private lazy var collectionView: UICollectionView = {
        let layout = UICollectionViewFlowLayout()
        layout.itemSize = CGSize(width: (ScreenWidth - margin * 2) * 0.5 - margin, height: 56)
        layout.headerReferenceSize = CGSize(width: ScreenWidth, height: 20 + 12)
        layout.minimumInteritemSpacing = margin
        layout.minimumLineSpacing = margin
        let view = UICollectionView(frame: .zero, collectionViewLayout: layout)
        view.backgroundColor = .clear
        view.isScrollEnabled = false
        view.contentInsetAdjustmentBehavior = .never
        view.register(TemplateSelectionViewCell.self, forCellWithReuseIdentifier: TemplateSelectionViewCell.cellId)
        view.register(TemplateSelectionHeaderView.self, forSupplementaryViewOfKind: UICollectionView.elementKindSectionHeader, withReuseIdentifier: TemplateSelectionHeaderView.viewId)
        view.register(UICollectionReusableView.self, forSupplementaryViewOfKind: UICollectionView.elementKindSectionFooter, withReuseIdentifier: "kFooterId")
        view.dataSource = self
        view.delegate = self
        view.contentInset = UIEdgeInsets(top: 0, left: 16, bottom: 20, right: 16)
        return view
    }()
}

extension TemplateSelectionView: UICollectionViewDataSource, UICollectionViewDelegate, UICollectionViewDelegateFlowLayout {
    func numberOfSections(in collectionView: UICollectionView) -> Int {
        dataSource.count
    }
    
    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        dataSource[section].toTempalateModeDataSource().count
    }
    
    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: TemplateSelectionViewCell.cellId, for: indexPath)
        if let cell = cell as? TemplateSelectionViewCell {
            let selectionMode = dataSource[indexPath.section]
            let mode = selectionMode.toTempalateModeDataSource()[indexPath.item]
            switch selectionMode {
            case .pk:
                cell.pkMode = mode
                cell.isSelectMode = currentPkMode == mode
            case .seat:
                cell.templateMode = mode
                cell.isSelectMode = currentMode == mode
            }
        }
        return cell
    }
    
    func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        let selectionMode = dataSource[indexPath.section]
        switch selectionMode {
        case .pk:
            currentPkMode = selectionMode.toTempalateModeDataSource()[indexPath.item]
            onSelectPkMode?(currentPkMode)
        case .seat:
            currentMode = selectionMode.toTempalateModeDataSource()[indexPath.item]
            onSelectMode?(currentMode)
        }
        collectionView.reloadData()
    }
    
    func collectionView(_ collectionView: UICollectionView, viewForSupplementaryElementOfKind kind: String, at indexPath: IndexPath) -> UICollectionReusableView {
        if kind == UICollectionView.elementKindSectionHeader {
            let headerView = collectionView.dequeueReusableSupplementaryView(ofKind: kind, withReuseIdentifier: TemplateSelectionHeaderView.viewId, for: indexPath)
            if let headerView = headerView as? TemplateSelectionHeaderView {
                let sectionMode = dataSource[indexPath.section]
                headerView.title = sectionMode.toString()
            }
            return headerView
        } else if kind == UICollectionView.elementKindSectionFooter {
            let footerView = collectionView.dequeueReusableSupplementaryView(ofKind: kind, withReuseIdentifier: "kFooterId", for: indexPath)
            footerView.backgroundColor = .clear
            return footerView
        }
        return UICollectionReusableView()
    }
    
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, referenceSizeForFooterInSection section: Int) -> CGSize {
        dataSource[section] == .seat ? .zero : CGSize(width: ScreenWidth, height: 25)
    }
}

class TemplateSelectionViewCell: UICollectionViewCell {
    static let cellId = "TemplateSelectionViewCellId"
    var templateMode: LiveTemplateMode? {
        didSet {
            if let mode = templateMode {
                imageView.image = internalImage(mode.toImageName())
                titleLabel.text = mode.toString()
            }
        }
    }
    
    var pkMode: LiveTemplateMode? {
        didSet {
            if let mode = pkMode {
                imageView.image = internalImage(mode.toPkImageName())
                titleLabel.text = mode.toString()
            }
        }
    }
    
    var isSelectMode: Bool = false {
        didSet {
            if isSelectMode {
                bgView.backgroundColor = UIColor(hex: "#243047")
                bgView.layer.borderWidth = 2
                bgView.layer.borderColor = UIColor(hex: "#2B6AD6")?.cgColor
            } else {
                bgView.backgroundColor = UIColor(hex: "#2B2C30")
                bgView.layer.borderWidth = 0
            }
        }
    }
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        initUI()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private lazy var imageView = UIImageView(frame: .zero)
    private lazy var titleLabel: UILabel = {
        let view = UILabel(frame: .zero)
        view.font = .customFont(ofSize: 14, weight: .regular)
        view.textColor = .white.withAlphaComponent(0.9)
        view.textAlignment = .left
        view.adjustsFontSizeToFitWidth = true
        view.minimumScaleFactor = 0.5
        view.baselineAdjustment = .alignCenters
        return view
    }()
    
    private lazy var bgView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = UIColor(hex: "#2B2C30")
        view.layer.cornerRadius = 8
        view.clipsToBounds = true
        return view
    }()
    
    private func initUI() {
        contentView.addSubview(bgView)
        bgView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        
        bgView.addSubview(imageView)
        imageView.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(12)
            make.centerY.equalToSuperview()
            make.size.equalTo(CGSize(width: 32, height: 32))
        }
        
        bgView.addSubview(titleLabel)
        titleLabel.snp.makeConstraints { make in
            make.leading.equalTo(imageView.snp.trailing).offset(12)
            make.trailing.equalToSuperview().offset(-12)
            make.centerY.equalToSuperview()
        }
    }
}

class TemplateSelectionHeaderView: UICollectionReusableView {
    static let viewId = "kTemplateSelectionHeaderViewId"
    
    var title: String = "" {
        didSet {
            titleLabel.text = title
        }
    }
    
    private lazy var titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 14, weight: .regular)
        label.textColor = .white.withAlphaComponent(0.55)
        return label
    }()
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        
        backgroundColor = .clear
        
        addSubview(titleLabel)
        titleLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(16)
            make.height.equalTo(20)
            make.bottom.equalToSuperview().offset(-12)
        }
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
}

enum SelectionMode: CaseIterable {
    case pk
    case seat
    func toTempalateModeDataSource() -> [LiveTemplateMode] {
        switch self {
        case .pk:
            return [.verticalGridDynamic, .verticalFloatDynamic]
        case .seat:
            return LiveTemplateMode.allCases
        }
    }
    func toString() -> String {
        switch self {
        case .pk:
            return "主播PK布局"
        case .seat:
            return "连观众布局"
        }
    }
}

private extension String {
    static let titleText = internalLocalized("Layout settings")
    static let anchorPkText = internalLocalized("Layout co-host")
    static let anchorLinkText = internalLocalized("Layout co-guest")
}
