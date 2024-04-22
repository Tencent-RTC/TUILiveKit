//
//  LinkMicAudienceFloatView.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/11/2.
//

import UIKit
import TUICore

class UserImageCell: UICollectionViewCell {
    var userInfo: UserInfo? {
        didSet {
            if let url = URL(string: userInfo?.avatarUrl.value ?? "") {
                avatarImageView.kf.setImage(with: url, placeholder: UIImage.placeholderImage)
            } else {
                avatarImageView.image = .placeholderImage
            }
        }
    }

    lazy var avatarImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.layer.cornerRadius = 40.scale375() * 0.5
        imageView.layer.masksToBounds = true
        contentView.addSubview(imageView)
        return imageView
    }()

    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else {
            return
        }
        isViewReady = true
        contentView.backgroundColor = .clear
        avatarImageView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    func setImage(image: UIImage?) {
        avatarImageView.kf.cancelDownloadTask()
        avatarImageView.image = image
    }
}

class LinkMicAudienceFloatView: UIView {
  
    private var dotsTimer: Timer = Timer()
    private var engineService: RoomEngineService
    private var liveRoomInfo:LiveRoomInfo {
          engineService.liveRoomInfo
    }
    private var isViewReady: Bool = false
    init(engineService: RoomEngineService) {
        self.engineService = engineService
        super.init(frame: .zero)
        updateLabelText()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        backgroundColor = .black
        constructViewHierarchy()
        activateConstraints()
        updateView()
        liveRoomInfo.audienceList.addObserver(self) { [weak self] _, _ in
            self?.updateView()
        }
    }

    let tipsLabel: UILabel = {
        let label = UILabel()
        label.textColor = .flowKitWhite
        label.text = .localizedReplace(.toBePassedText, replace: "")
        label.font = .customFont(ofSize: 14)
        label.sizeToFit()
        label.adjustsFontSizeToFitWidth = true
        if TUIGlobalization.getRTLOption() {
            label.textAlignment = .right
        } else {
            label.textAlignment = .left
        }
        return label
    }()

    lazy var collectionView: UICollectionView = {
        let layout = UICollectionViewFlowLayout()
        layout.scrollDirection = .horizontal
        layout.itemSize = CGSize(width: 40.scale375(), height: 40.scale375())
        let collectionView = UICollectionView(frame: .zero, collectionViewLayout: layout)
        collectionView.showsVerticalScrollIndicator = false
        collectionView.showsHorizontalScrollIndicator = false
        collectionView.backgroundColor = .clear
        collectionView.isUserInteractionEnabled = true
        collectionView.contentMode = .scaleToFill
        collectionView.dataSource = self
        collectionView.register(UserImageCell.self, forCellWithReuseIdentifier: UserImageCell.cellReuseIdentifier)
        let tap = UITapGestureRecognizer(target: self, action: #selector(tapAction))
        collectionView.addGestureRecognizer(tap)
        return collectionView
    }()

    private func updateView() {
        collectionView.reloadData()
    }

    private func updateLabelText() {
        var dots = ""
        dotsTimer = Timer(timeInterval: 1.0, repeats: true) { [weak self] _ in
            guard let self = self else { return }
            if dots.count == 3 {
                dots.removeAll()
            }else {
                dots.append(".")
            }
            self.tipsLabel.text? = .localizedReplace(.toBePassedText, replace: dots)
        }
        RunLoop.current.add(dotsTimer, forMode: .default)
    }

    deinit {
        dotsTimer.invalidate()
    }
}

// MARK: Layout

extension LinkMicAudienceFloatView {
    func constructViewHierarchy() {
        backgroundColor = .g2.withAlphaComponent(0.4)
        layer.cornerRadius = 10
        layer.masksToBounds = true
        layer.borderWidth = 1
        layer.borderColor = UIColor.flowKitWhite.withAlphaComponent(0.2).cgColor
        
        addSubview(tipsLabel)
        addSubview(collectionView)
    }

    func activateConstraints() {
        collectionView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(14.scale375Width())
            make.height.width.equalTo(40.scale375())
            make.centerX.equalToSuperview()
        }

        tipsLabel.snp.makeConstraints { make in
            make.top.equalTo(collectionView.snp.bottom).offset(6.scale375Width())
            make.centerX.equalToSuperview()
            make.width.equalTo(55.scale375())
            make.height.equalTo(20.scale375())
        }
    }
}

// MARK: - UICollectionViewDataSource

extension LinkMicAudienceFloatView: UICollectionViewDataSource {
    func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 1
    }

    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return 1
    }

    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: UserImageCell.cellReuseIdentifier,
                                                      for: indexPath) as! UserImageCell
        cell.userInfo = engineService.liveKitStore.selfInfo
        return cell
    }
}

// MARK: - UICollectionViewDelegate

extension LinkMicAudienceFloatView: UICollectionViewDelegateFlowLayout {
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout:
                        UICollectionViewLayout, insetForSectionAt section: Int) -> UIEdgeInsets {
        let width = collectionView.frame.width
        let margin = width * 0.3
        return UIEdgeInsets(top: 10, left: margin / 2, bottom: 10, right: margin / 2)
    }
}

// MARK: Action

extension LinkMicAudienceFloatView {
    @objc func tapAction() {
        showCancelLinkMicPanel()
    }
    
    private func showCancelLinkMicPanel() {
        let model = ActionModel()
        let designConfig = ActionItemDesignConfig(lineWidth: 7, titleColor: .redPinkColor)
        designConfig.backgroundColor = .g2
        designConfig.lineColor = .g3.withAlphaComponent(0.1)
        let item = ActionItem(text: .cancelLinkMicRequestText, designConfig: designConfig, action: AudienceViewActionEvent.didCancelRequestLinkClick)
        model.items.append(item)
        
        let actionPanel = ActionPanel(model: model)
        PopupPanelController.alertView(actionPanel)
        actionPanel.clickEventCallBack.addObserver(self) { [weak self] action, _ in
            guard let self = self else{ return}
            guard let actionType = action as? AudienceViewActionEvent else{ return}
            if actionType == .didCancelRequestLinkClick {
                self.engineService.cancelRequest(self.engineService.liveKitStore.selfInfo.requestId)
            }
        }
    }
}

private extension String {
    static var toBePassedText: String {
        localized("live.audience.link.float.toBePassed.xxx")
    }
    static var cancelLinkMicRequestText = {
        localized("live.audience.link.confirm.cancelLinkMicRequest")
    }()
}
