//
//  RoomInfoViewPanel.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/23.
//

import Foundation
import TUICore

class ListCellItemData {
    var titleText: String = ""
    var messageText: String = ""
    var normalTitle: String = ""
    var normalImage: UIImage?
    var hasButton: Bool = false
}

class ListCellItemView: UIView {
    let itemData: ListCellItemData
    
    let titleLabel: UILabel = {
        let view = UILabel()
        view.adjustsFontSizeToFitWidth = false
        view.backgroundColor = .clear
        view.textColor = .g5
        view.font = .customFont(ofSize: 14,weight: .medium)
        if TUIGlobalization.getRTLOption() {
            view.textAlignment = .right
        } else {
            view.textAlignment = .left
        }
        return view
    }()
    
    let messageLabel: UILabel = {
        let view = UILabel()
        view.backgroundColor = .clear
        view.textColor = .g7
        view.font = .customFont(ofSize: 14,weight: .medium)
        view.adjustsFontSizeToFitWidth = false
        if TUIGlobalization.getRTLOption() {
            view.textAlignment = .right
        } else {
            view.textAlignment = .left
        }
        return view
    }()

    lazy var rightButton: UIButton = {
        let button = UIButton(type: .custom)
        button.setImage(itemData.normalImage, for: .normal)
        button.setTitle(itemData.normalTitle, for: .normal)
        button.titleLabel?.font = .customFont(ofSize: 10)
        
        button.titleLabel?.tintColor = .g5
        button.backgroundColor = .g4
        button.layer.cornerRadius = 6
        button.layer.masksToBounds = true
        button.imageEdgeInsets = UIEdgeInsets(top: 0, left: -10, bottom: 0, right: 0)
        button.titleEdgeInsets = UIEdgeInsets(top: 0, left: 5, bottom: 0, right: 0)
        button.addTarget(self, action: #selector(rightButtonClick), for: .touchUpInside)
        return button
    }()
    
    init(itemData: ListCellItemData) {
        self.itemData = itemData
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
        constructViewHierarchy()
        activateConstraints()
        updateView()
    }
    
    func constructViewHierarchy() {
        addSubview(titleLabel)
        addSubview(messageLabel)
        addSubview(rightButton)
    }
    
    func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(20)
            make.centerY.equalToSuperview()
            make.width.equalTo(100.scale375())
        }
        
        messageLabel.snp.makeConstraints { make in
            make.left.equalTo(titleLabel.snp.right).offset(10.scale375())
            make.trailing.equalToSuperview().inset(92)
            make.centerY.equalToSuperview()
        }
        
        rightButton.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.width.equalTo(64)
            make.height.equalTo(22)
            make.trailing.equalToSuperview().inset(16)
        }
    }
    
    func updateView() {
        titleLabel.text = itemData.titleText
        titleLabel.isHidden = itemData.titleText.isEmpty
        messageLabel.isHidden = itemData.messageText.isEmpty
        messageLabel.text = itemData.messageText
        rightButton.isHidden = !itemData.hasButton
       
    }
    
    @objc func rightButtonClick() {
        UIPasteboard.general.string = messageLabel.text
        makeToast(.roomInfoCopySuccessTitle)
    }
    
    deinit {
        debugPrint("deinit \(self)")
    }
}


class RoomInfoViewPanel: UIView {
    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()

    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
    }

    private var popupAction: Observable<PopupPanelAction>?
    private var engineService: RoomEngineService
    private var liveRoomInfo:LiveRoomInfo {
          engineService.liveRoomInfo
    }
    init(engineService: RoomEngineService) {
        self.engineService = engineService
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    lazy var nameLabel: UILabel = {
        let label = UILabel()
        label.contentMode = .center
        label.font = .customFont(ofSize: 16, weight: .medium)
        label.textColor = .g7
        label.text = .localizedReplace(.roomName, replace: self.liveRoomInfo.name.value)
        return label
    }()
    
    let stackView: UIStackView = {
        let view = UIStackView()
        view.axis = .vertical
        view.alignment = .center
        view.spacing = 0
        view.backgroundColor = .clear
        return view
    }()
    
    
    func constructViewHierarchy() {
        backgroundColor = .g2
        layer.cornerRadius = 16
        layer.masksToBounds = true
        addSubview(nameLabel)
        addSubview(stackView)
        for item in items {
            let view = ListCellItemView(itemData: item)
            stackView.addArrangedSubview(view)
            view.snp.makeConstraints { make in
                make.height.equalTo(30.scale375())
                make.width.equalToSuperview()
            }
        }
    }
    
    func activateConstraints() {
        snp.remakeConstraints { make in
            if isPortrait {
                make.width.equalToSuperview()
                make.height.equalTo(300.scale375Height())
                make.centerX.equalToSuperview()
            } else {
                make.width.equalTo(375)
                make.height.equalToSuperview()
                make.trailing.equalToSuperview()
            }
            make.bottom.equalToSuperview()
        }

        nameLabel.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(20.scale375Height())
            make.centerX.equalToSuperview()
            make.height.equalTo(24.scale375Height())
        }

        stackView.snp.remakeConstraints { make in
            make.leading.trailing.equalToSuperview()
            make.top.equalTo(nameLabel.snp.bottom).offset(20)
            make.height.equalTo(CGFloat(items.count) * 30.scale375())
        }
    }
   
    lazy var items : [ListCellItemData] = {
        var items:[ListCellItemData] = []
        
        var item = ListCellItemData()
        item.titleText = .roomInfoOwnerTitle
        item.messageText = liveRoomInfo.anchorInfo.value.name.value
        items.append(item)
        
        item = ListCellItemData()
        item.titleText = .roomInfoIdTitle
        item.messageText = liveRoomInfo.roomId.value
        item.normalImage = .liveBundleImage("live_copy_icon")
        item.normalTitle = .roomInfoCopyTitle
        item.hasButton = true
        items.append(item)
        
        return items
    }()
}

extension RoomInfoViewPanel: PopupPanelSubViewProtocol {
    func setAction(_ popupAction: Observable<PopupPanelAction>) {
        self.popupAction = popupAction
    }

    func updateRootViewOrientation(isPortrait: Bool) {
        self.isPortrait = isPortrait
        activateConstraints()
    }
}

private extension String {
    static var roomName: String {
        localized("live.room.name.title.xxx")
    }
    
    static var roomInfoOwnerTitle: String {
        localized("live.room.info.owner.title")
    }
    
    static var roomInfoCategoryTitle: String {
        localized("live.room.info.category.title")
    }
    
    static var roomInfoIdTitle: String {
        localized("live.room.info.id.title")
    }
    
    static var roomInfoCopyTitle: String {
        localized("live.room.info.copy.title")
    }
    
    static var roomInfoCopySuccessTitle: String {
        localized("live.room.info.copy.success.title")
    }
}
