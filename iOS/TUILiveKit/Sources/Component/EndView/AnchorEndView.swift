//
//  AnchorEndView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2024/4/17.
//

import TUICore
import UIKit
import RTCCommon

class LiveDataModel {
    let roomId: String
    let liveDuration:Int
    var audienceCount:Int
    let messageCount:Int
    let giftIncome:Int
    let giftPeopleCount:Int
    let likeCount:Int
    init(roomId: String, liveDuration: Int, audienceCount: Int, messageCount: Int, giftIncome: Int, giftPeopleCount: Int, likeCount: Int) {
        self.roomId = roomId
        self.liveDuration = liveDuration
        self.audienceCount = audienceCount
        self.messageCount = messageCount
        self.giftIncome = giftIncome
        self.giftPeopleCount = giftPeopleCount
        self.likeCount = likeCount
    }
}

class AnchorEndView: UIView {
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }
    
    weak var delegate: LiveEndViewDelegate?

    let liveDataModel: LiveDataModel
    init(liveDataModel: LiveDataModel) {
        self.liveDataModel = liveDataModel
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    func updateAudienceCount(_ audienceCount: Int) {
        liveDataModel.audienceCount = audienceCount
        collectionView.reloadData()
    }
    
    private lazy var titleLabel: UILabel = {
        let label = UILabel()
        label.textAlignment = .center
        label.font = .customFont(ofSize: 20)
        label.textColor = .flowKitWhite
        label.text = .titleText
        return label
    }()

    private lazy var closeButton: UIButton = {
        let button = UIButton()
        button.setImage(.liveBundleImage("live_leave_icon"), for: .normal)
        button.addTarget(self, action: #selector(closeButtonClick), for: .touchUpInside)
        return button
    }()

    private let cellID = "AnchorEndViewCellID"
    private lazy var collectionView: UICollectionView = {
        let layout = UICollectionViewFlowLayout()
        layout.minimumLineSpacing = 0
        layout.minimumInteritemSpacing = 0
        let collectionView = UICollectionView(frame: .zero, collectionViewLayout: layout)
        collectionView.dataSource = self
        collectionView.delegate = self
        collectionView.backgroundColor = .clear
        collectionView.isScrollEnabled = false
        collectionView.contentInsetAdjustmentBehavior = .never
        collectionView.register(UICollectionViewCell.self, forCellWithReuseIdentifier: cellID)
        return collectionView
    }()
    
    private lazy var contentBgView: UIView = {
        let content = UIView()
        content.backgroundColor = .blue40Transparency
        content.layer.cornerRadius = 20.scale375()
        return content
    }()
    
    private lazy var contentDescLabel: UILabel = {
        let label = UILabel()
        label.textAlignment = .left
        label.font = .customFont(ofSize: 14)
        label.textColor = .flowKitWhite
        label.text = .contentDestText
        return label
    }()
    
    private func getTitleWithIndex(_ index: Int) -> (String, String) {
        switch index {
        case 0: // liveDurationCell
            let minutes = liveDataModel.liveDuration / 60
            let remainingSeconds = liveDataModel.liveDuration % 60
            let topStr = String(format: "%02d:%02d", minutes, remainingSeconds)
            return (topTitle: topStr, bottomTitle: .durationText)
        case 1: // audienceCountCell
            return (topTitle: "\(liveDataModel.audienceCount)", bottomTitle: .audienceCountText)
        case 2: // messageCountCell
            return (topTitle: "\(liveDataModel.messageCount)", bottomTitle: .messageCountText)
        case 3: // giftIncomeCell
            return (topTitle: "\(liveDataModel.giftIncome)", bottomTitle: .giftIncomeText)
        case 4: // giftPeopleCountCell
            return (topTitle: "\(liveDataModel.giftPeopleCount)", bottomTitle: .giftPeopleCountText)
        case 5: // likeCountCell
            return (topTitle: "\(liveDataModel.likeCount)", bottomTitle: .likeCountText)
        default:
            return ("", "")
        }
    }
}

// MARK: Layout

extension AnchorEndView {
    
    private func constructViewHierarchy() {
        backgroundColor = .g2
        addSubview(titleLabel)
        addSubview(closeButton)
        addSubview(contentBgView)
        contentBgView.addSubview(contentDescLabel)
        contentBgView.addSubview(collectionView)
    }

    private func activateConstraints() {
        
        closeButton.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(64.scale375Height())
            make.trailing.equalToSuperview().offset(-30)
            make.width.height.equalTo(30.scale375())
        }
        
        titleLabel.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalToSuperview().offset(120.scale375Height())
            make.width.equalToSuperview()
            make.height.equalTo(30.scale375Height())
        }
        
        contentBgView.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview().inset(16.scale375())
            make.height.equalTo(160.scale375Height())
            make.top.equalTo(titleLabel.snp.bottom).offset(50.scale375Height())
        }
        
        contentDescLabel.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview().inset(16.scale375())
            make.top.equalToSuperview().inset(5.scale375Height())
        }
        
        collectionView.snp.makeConstraints { make in
            make.height.equalTo((70*2).scale375Height())
            make.leading.trailing.equalToSuperview()
            make.top.equalToSuperview().offset(20.scale375Height())
        }
    }
    
    private func createContentCell(topTitle:String , bottomTitle:String) -> UIStackView {
        let topLabel = UILabel()
        topLabel.textAlignment = .center
        topLabel.font = .customFont(ofSize: 12)
        topLabel.textColor = .flowKitWhite
        topLabel.text = topTitle
        
        let botomLabel = UILabel()
        botomLabel.textAlignment = .center
        botomLabel.font = .customFont(ofSize: 12)
        botomLabel.textColor = .g5
        botomLabel.text = bottomTitle
        
        let stackView = UIStackView(arrangedSubviews: [topLabel, botomLabel])
        stackView.axis = .vertical
        stackView.spacing = -25.scale375Height()
        stackView.distribution = .fillEqually
        stackView.alignment = .fill
        return stackView
    }
    
}

extension AnchorEndView: UICollectionViewDataSource, UICollectionViewDelegate, UICollectionViewDelegateFlowLayout {
    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return 6
    }
    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: cellID, for: indexPath)
        cell.contentView.subviews.forEach { $0.removeFromSuperview() }
        cell.backgroundColor = .clear
        cell.contentView.backgroundColor = .clear
        
        let titles = getTitleWithIndex(indexPath.item)
        let view = createContentCell(topTitle: titles.0, bottomTitle: titles.1)
        cell.contentView.addSubview(view)
        view.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        return cell
    }
    
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, sizeForItemAt indexPath: IndexPath) -> CGSize {
        return CGSize(width: collectionView.bounds.width / 3, height: 70.scale375Height())
    }
}

// MARK: Action

extension AnchorEndView {
    @objc func closeButtonClick() {
        delegate?.onCloseButtonClick()
    }
}

private extension String {
    static var titleText: String {
        localized("Live broadcast has ended")
    }
    
    static var contentDestText: String {
        localized("Live data")
    }
    
    static var durationText: String {
        localized("Duration")
    }
    
    static var giftIncomeText: String {
        localized("Gift Income")
    }
    
    static var audienceCountText: String {
        localized("Total Views")
    }
    
    static var messageCountText: String {
        localized("Messages")
    }
    
    static var giftPeopleCountText: String {
        localized("Gift givers")
    }
    
    static var likeCountText: String {
        localized("Likes")
    }
}
