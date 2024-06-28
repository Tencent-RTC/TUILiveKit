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
    let audienceCount:Int
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
    private let routerStore: RouterStore
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }

    let liveDataModel: LiveDataModel
    init(liveDataModel: LiveDataModel, routerStore: RouterStore) {
        self.liveDataModel = liveDataModel
        self.routerStore = routerStore
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
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
        button.setImage(.liveBundleImage("live_audience_close_icon"), for: .normal)
        button.addTarget(self, action: #selector(closeButtonClick), for: .touchUpInside)
        return button
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
    
    private lazy var liveDurationCell: UIView = {
        let minutes = liveDataModel.liveDuration / 60
        let remainingSeconds = liveDataModel.liveDuration % 60
        let topStr = String(format: "%02d:%02d", minutes, remainingSeconds)
        return createContentCell(topTitle: topStr, bottomTitle: .durationText)
    }()
    
    private lazy var audienceCountCell: UIView = {
        return createContentCell(topTitle: "\(liveDataModel.audienceCount)", bottomTitle: .audienceCountText)
    }()
    
    private lazy var messageCountCell: UIView = {
        return createContentCell(topTitle: "\(liveDataModel.messageCount)", bottomTitle: .messageCountText)
    }()
    
    private lazy var giftIncomeCell: UIView = {
        return createContentCell(topTitle: "\(liveDataModel.giftIncome)", bottomTitle: .giftIncomeText)
    }()
    
    private lazy var giftPeopleCountCell: UIView = {
        return createContentCell(topTitle: "\(liveDataModel.giftPeopleCount)", bottomTitle: .giftPeopleCountText)
    }()
    
    private lazy var likeCountCell: UIView = {
        return createContentCell(topTitle: "\(liveDataModel.likeCount)", bottomTitle: .likeCountText)
    }()
    
}

// MARK: Layout

extension AnchorEndView {
    
    private func constructViewHierarchy() {
        backgroundColor = .g2
        addSubview(titleLabel)
        addSubview(closeButton)
        addSubview(contentBgView)
        contentBgView.addSubview(contentDescLabel)
        contentBgView.addSubview(liveDurationCell)
        contentBgView.addSubview(audienceCountCell)
        contentBgView.addSubview(messageCountCell)
        contentBgView.addSubview(giftIncomeCell)
        contentBgView.addSubview(giftPeopleCountCell)
        contentBgView.addSubview(likeCountCell)
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
        
        liveDurationCell.snp.makeConstraints { make in
            make.height.equalTo(70.scale375Height())
            make.width.equalToSuperview().multipliedBy(1.0/3.0)
            make.leading.equalToSuperview()
            make.top.equalToSuperview().offset(20.scale375Height())
        }
        
        audienceCountCell.snp.makeConstraints { make in
            make.height.equalTo(70.scale375Height())
            make.width.equalToSuperview().multipliedBy(1.0/3.0)
            make.centerX.equalToSuperview()
            make.top.equalToSuperview().offset(20.scale375Height())
        }
        
        messageCountCell.snp.makeConstraints { make in
            make.height.equalTo(70.scale375Height())
            make.width.equalToSuperview().multipliedBy(1.0/3.0)
            make.trailing.equalToSuperview()
            make.top.equalToSuperview().offset(20.scale375Height())
        }
        
        giftIncomeCell.snp.makeConstraints { make in
            make.height.equalTo(70.scale375Height())
            make.width.equalToSuperview().multipliedBy(1.0/3.0)
            make.leading.equalToSuperview()
            make.top.equalToSuperview().offset(90.scale375Height())
        }
        
        giftPeopleCountCell.snp.makeConstraints { make in
            make.height.equalTo(70.scale375Height())
            make.width.equalToSuperview().multipliedBy(1.0/3.0)
            make.centerX.equalToSuperview()
            make.top.equalToSuperview().offset(90.scale375Height())
        }
        
        likeCountCell.snp.makeConstraints { make in
            make.height.equalTo(70.scale375Height())
            make.width.equalToSuperview().multipliedBy(1.0/3.0)
            make.trailing.equalToSuperview()
            make.top.equalToSuperview().offset(90.scale375Height())
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

// MARK: Action

extension AnchorEndView {
    @objc func closeButtonClick() {
        routerStore.router(action: .exit)
    }
}

private extension String {
    static var titleText: String {
        localized("live.audience.mask.title")
    }
    
    static var contentDestText: String {
        localized("live.anchor.live.data")
    }
    
    static var durationText: String {
        localized("live.anchor.live.data.duration")
    }
    
    static var giftIncomeText: String {
        localized("live.anchor.live.data.gift.income")
    }
    
    static var audienceCountText: String {
        localized("live.anchor.live.data.audience.count")
    }
    
    static var messageCountText: String {
        localized("live.anchor.live.data.message.count")
    }
    
    static var giftPeopleCountText: String {
        localized("live.anchor.live.data.gift.people.count")
    }
    
    static var likeCountText: String {
        localized("live.anchor.live.data.like.count")
    }
}
