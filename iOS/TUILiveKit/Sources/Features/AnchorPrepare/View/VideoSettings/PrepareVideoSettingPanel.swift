//
//  PrepareVideoSettingPanel.swift
//  AFNetworking
//
//  Created by jack on 2025/8/28.
//

import UIKit
import Foundation
import RTCCommon
import AtomicXCore
import RTCRoomEngine
import Combine

class PrepareVideoSettingPanel: UIView {
    
    enum VideoSettingType {
        case mirror
        case resolution
        
        var cellId: String {
            if self == .mirror {
                return  PrepareVideoSettingSwitchCell.identifier
            }
            if self == .resolution {
                return PrepareVideoSettingPullDownCell.identifier
            }
            return ""
        }
    }
    
    private weak var coreView: LiveCoreView?
    
    private let titleLabel: UILabel = {
        let view = UILabel()
        view.text = .settingTitleText
        view.textColor = .textPrimaryColor
        view.font = .customFont(ofSize: 16, weight: .medium)
        view.textAlignment = .center
        return view
    }()
    
    private lazy var tableView: UITableView = {
        let view = UITableView(frame: .zero, style: .plain)
        view.dataSource = self
        view.delegate = self
        view.register(PrepareVideoSettingSwitchCell.self, forCellReuseIdentifier: PrepareVideoSettingSwitchCell.identifier)
        view.register(PrepareVideoSettingPullDownCell.self, forCellReuseIdentifier: PrepareVideoSettingPullDownCell.identifier)
        view.separatorStyle = .none
        view.backgroundColor = .bgEntrycardColor
        view.sectionFooterHeight = 0
        view.sectionHeaderHeight = 0
        view.showsVerticalScrollIndicator = false
        view.layer.cornerRadius = 8
        view.layer.masksToBounds = true
        return view
    }()
    
    private var videoQuality: TUIVideoQuality = .quality1080P
    private var items: [VideoSettingType] = [.mirror, .resolution]
    private weak var popupViewController: PopupViewController?
    
    public init(coreView: LiveCoreView) {
        self.coreView = coreView
        super.init(frame: .zero)
        backgroundColor = .bgOperateColor
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }
}

// MARK: - Layout
private extension PrepareVideoSettingPanel {
    
    func constructViewHierarchy() {
        addSubview(titleLabel)
        addSubview(tableView)
    }
    
    func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(20)
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(24)
        }
        tableView.snp.makeConstraints { make in
            make.top.equalTo(titleLabel.snp.bottom).offset(20)
            make.bottom.equalToSuperview().inset(16)
            make.height.equalTo(112)
            make.leading.trailing.equalToSuperview().inset(16)
        }
    }
}

// MARK: - Action
private extension PrepareVideoSettingPanel {
    
    func enableMirror(_ enable: Bool) {
        coreView?.enableMirror(enable: enable)
    }
    
    func selectResolution() {
        let view = VideoQualitySelectionPanel(resolutions: [.quality1080P, .quality720P])
        view.cancelClosure = { [weak self] in
            guard let self = self else { return }
            self.popupViewController?.dismiss(animated: true)
        }
        view.selectedClosure = { [weak self] (quality) in
            guard let self = self else { return }
            TUIRoomEngine.sharedInstance().updateVideoQuality(quality)
            self.videoQuality = quality
            self.tableView.reloadData()
            self.popupViewController?.dismiss(animated: true)
        }
        let menuContainerView = MenuContainerView(contentView: view, safeBottomViewBackgroundColor: .bgOperateColor)
        let popupViewController = PopupViewController(contentView: menuContainerView, supportBlurView: false)
        menuContainerView.blackAreaClickClosure = { [weak self] in
            guard let self = self else { return }
            self.popupViewController?.dismiss(animated: true)
        }
        guard let presentingViewController = getCurrentViewController() else { return }
        presentingViewController.present(popupViewController, animated: true)
        self.popupViewController = popupViewController
    }
}

// MARK: - UITableViewDataSource
extension PrepareVideoSettingPanel: UITableViewDataSource {
    
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return items.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let item = items[indexPath.row]
        let cell = tableView.dequeueReusableCell(withIdentifier: item.cellId,
                                                 for: indexPath)
        if item == .mirror, let mirrorCell = cell as? PrepareVideoSettingSwitchCell {
            mirrorCell.titleLabel.text = .mirrorText
            bindViewState(cell: mirrorCell)
            mirrorCell.switchBlock = { [weak self] (isOn) in
                guard let self = self else { return }
                self.enableMirror(isOn)
            }
        }
        if item == .resolution, let resolutionCell = cell as? PrepareVideoSettingPullDownCell {
            resolutionCell.titleLabel.text = .resolutionText
            resolutionCell.contentLabel.text = .videoQualityToString(quality: videoQuality)
            resolutionCell.clickBlock = { [weak self] in
                guard let self = self else { return }
                self.selectResolution()
            }
        }
        return cell
    }
    
    private func bindViewState(cell: PrepareVideoSettingSwitchCell) {
        coreView?.subscribeState(StatePublisherSelector(keyPath: \MediaState.isMirrorEnabled))
            .receive(on: RunLoop.main)
            .sink { [weak cell] isMirrorEnable in
                guard let cell = cell else { return }
                cell.configSwitch.isOn = isMirrorEnable
            }
            .store(in: &cell.cancellableSet)
    }
}

// MARK: - UITableViewDelegate
extension PrepareVideoSettingPanel: UITableViewDelegate {
    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return 56.0
    }
}

class PrepareVideoSettingSwitchCell: UITableViewCell {
    
    static let identifier = "VideoSettingsSwitchCell"
    var cancellableSet = Set<AnyCancellable>()
    public var switchBlock: ((Bool)->())?
    
    public let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textAlignment = .center
        label.font = .customFont(ofSize: 16.0, weight: .medium)
        label.textColor = .g7
        return label
    }()
    
    public let configSwitch: UISwitch = {
        let view = UISwitch()
        view.onTintColor = .b1
        return view
    }()
    
    private var isViewReady = false
    public override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        setupStyle()
        isViewReady = true
    }
    
    override func prepareForReuse() {
        super.prepareForReuse()
        cancellableSet.removeAll()
    }
    
    private func constructViewHierarchy() {
        contentView.addSubview(titleLabel)
        contentView.addSubview(configSwitch)
    }
    
    private func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(14)
            make.centerY.equalToSuperview()
        }
        configSwitch.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-14)
            make.centerY.equalToSuperview()
        }
    }
    
    private func bindInteraction() {
        configSwitch.addTarget(self, action: #selector(switchAction(sender:)), for: .touchUpInside)
    }
    
    private func setupStyle() {
        backgroundColor = .clear
        selectionStyle = .none
    }
    
    @objc
    private func switchAction(sender: UISwitch) {
        switchBlock?(sender.isOn)
    }
}


class PrepareVideoSettingPullDownCell: UITableViewCell {
    public static let identifier = "VideoSettingsPullDownCell"
    
    public var clickBlock: (()->())?
    
    let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textAlignment = .center
        label.font = .customFont(ofSize: 16.0, weight: .medium)
        label.textColor = .g7
        return label
    }()
    
    let pullDownView: UIView = {
        let view = UIView(frame: .zero)
        return view
    }()
    
    let contentLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 16)
        label.textColor = .g7
        return label
    }()
    
    let arrowImageView: UIImageView = {
        let view = UIImageView(image: internalImage("live_drop_down_arrow"))
        view.contentMode = .center
        return view
    }()
    
    private var isViewReady = false
    public override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        setupStyle()
        isViewReady = true
    }
    
    private func constructViewHierarchy() {
        contentView.addSubview(titleLabel)
        contentView.addSubview(pullDownView)
        pullDownView.addSubview(contentLabel)
        pullDownView.addSubview(arrowImageView)
    }
    
    private func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(14)
            make.centerY.equalToSuperview()
        }
        pullDownView.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-14)
            make.centerY.equalToSuperview()
            
        }
        contentLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview()
            make.top.bottom.equalToSuperview()
        }
        arrowImageView.snp.makeConstraints { make in
            make.width.height.equalTo(20)
            make.trailing.equalToSuperview()
            make.centerY.equalTo(contentLabel)
            make.leading.equalTo(contentLabel.snp.trailing)
        }
    }
    
    private func bindInteraction() {
        pullDownView.addTapGesture(target: self, action: #selector(clickAction(sender:)))
    }
    
    private func setupStyle() {
        backgroundColor = .clear
        selectionStyle = .none
    }
    
    @objc
    private func clickAction(sender: UIButton) {
        clickBlock?()
    }
}


fileprivate extension String {
    static let settingTitleText: String = internalLocalized("Video settings")
    
    static let mirrorText: String = internalLocalized("Mirror")
    static let resolutionText: String = internalLocalized("Resolution")
    
    static func videoQualityToString(quality: TUIVideoQuality) -> String {
        switch quality {
        case .quality1080P:
            return "1080P"
        case .quality720P:
            return "720P"
        case .quality540P:
            return "540P"
        case .quality360P:
            return "360P"
        default:
            return "unknown"
        }
    }
    
}

