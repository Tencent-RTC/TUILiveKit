//
//  NetWorkInfoView.swift
//  Pods
//
//  Created by ssc on 2025/5/12.
//

import UIKit
import SnapKit
import Combine
import RTCCommon
import RTCRoomEngine
#if canImport(TXLiteAVSDK_TRTC)
import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
import TXLiteAVSDK_Professional
#endif

enum NetWorkInfoItemViewType {
    case video
    case audio
    case temperature
    case network
}

class NetWorkInfoView: UIView {
    private var cancellables = Set<AnyCancellable>()
    private weak var presentedPanelController: UIViewController?
    private let isAudience: Bool
    private weak var manager: NetWorkInfoManager?
    private weak var popupViewController: UIViewController?
    private let titleLabel: UILabel = {
        let label = UILabel()
        label.text = .liveInfoTitle
        label.font = UIFont(name: "pingFangSC-Medium", size: 16)
        label.textColor = UIColor.white.withAlphaComponent(0.9)
        return label
    }()

    private let containerView: UIView = {
        let view = UIView()
        view.backgroundColor = .bgOperateColor
        view.layer.shadowColor = UIColor.black.cgColor
        view.layer.shadowOffset = CGSize(width: 0, height: -2)
        view.layer.shadowOpacity = 0.06
        view.layer.shadowRadius = 6
        view.layer.cornerRadius = 10
        return view
    }()

    private let bottomView: UIView = {
        let view = UIView()
        view.backgroundColor = UIColor(hex: "#2B2C30")
        view.layer.cornerRadius = 10
        return view
    }()

    private let networkStatsContainer: UIView = {
        let view = UIView()
        return view
    }()

    private let rttView: UIView = {
        let view = UIView()
        return view
    }()

    private let rttValueLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont(name: "pingFangSC-Medium", size: 16)
        label.textColor = .greenColor
        return label
    }()

    private let rttTitleLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont(name: "pingFangSC-Regular", size: 12)
        label.textColor = UIColor.white.withAlphaComponent(0.55)
        label.text = .roundTripDelay
        return label
    }()

    private let downLossView: UIView = {
        let view = UIView()
        return view
    }()

    private let downLossValueLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont(name: "pingFangSC-Medium", size: 16)
        label.textColor = UIColor.white.withAlphaComponent(0.9)
        return label
    }()

    private let downLossTitleLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont(name: "pingFangSC-Regular", size: 12)
        label.textColor = UIColor.white.withAlphaComponent(0.55)
        label.text = .downlinkLoss
        return label
    }()

    private let upLossView: UIView = {
        let view = UIView()
        return view
    }()

    private let upLossValueLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont(name: "pingFangSC-Medium", size: 16)
        label.textColor = UIColor.white.withAlphaComponent(0.9)
        return label
    }()

    private let upLossTitleLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont(name: "pingFangSC-Regular", size: 12)
        label.textColor = UIColor.white.withAlphaComponent(0.55)
        label.text = .uplinkLoss
        return label
    }()


    private let tableView: UITableView = {
        let tableView = UITableView()
        tableView.backgroundColor = .clear
        tableView.separatorStyle = .none
        tableView.isScrollEnabled = false
        tableView.register(NetWorkInfoItemCell.self, forCellReuseIdentifier: "NetWorkInfoItemCell")
        return tableView
    }()
    
    private var items: [NetWorkInfoItemViewType] = []
    
    init(manager: NetWorkInfoManager, isAudience: Bool = false) {
        self.manager = manager
        self.isAudience = isAudience
        if isAudience == false {
            items.append(.video)
            items.append(.audio)
        }
        items.append(.temperature)
        items.append(.network)
        super.init(frame: .zero)
    }

    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        isViewReady = true
    }

    private func constructViewHierarchy() {
        addSubview(containerView)
        addSubview(titleLabel)
        containerView.addSubview(tableView)
        containerView.addSubview(bottomView)

        bottomView.addSubview(networkStatsContainer)
        networkStatsContainer.addSubview(rttView)
        rttView.addSubview(rttValueLabel)
        rttView.addSubview(rttTitleLabel)

        networkStatsContainer.addSubview(downLossView)
        downLossView.addSubview(downLossValueLabel)
        downLossView.addSubview(downLossTitleLabel)

        networkStatsContainer.addSubview(upLossView)
        upLossView.addSubview(upLossValueLabel)
        upLossView.addSubview(upLossTitleLabel)
    }

    private func activateConstraints() {
        containerView.snp.makeConstraints { make in
            make.left.right.equalTo(safeAreaLayoutGuide)
            make.top.equalToSuperview().inset(20.scale375())
            make.bottom.equalToSuperview()
        }

        titleLabel.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalToSuperview().offset(32.scale375())
        }

        tableView.snp.makeConstraints { make in
            make.left.right.equalToSuperview().inset(20.scale375())
            make.top.equalTo(titleLabel.snp.bottom).offset(12.scale375())
            make.height.equalTo(CGFloat(items.count) * 66.scale375())
        }


        bottomView.snp.makeConstraints { make in
            make.width.equalTo(309.scale375())
            make.height.equalTo(64.scale375())
            make.centerX.equalToSuperview()
            make.top.equalTo(tableView.snp.bottom).offset(20.scale375())
            make.bottom.equalToSuperview().inset(20.scale375())
        }

        networkStatsContainer.snp.makeConstraints { make in
            make.center.equalToSuperview()
            make.width.equalTo(303.scale375())
            make.height.equalTo(48.scale375())
        }

        rttView.snp.makeConstraints { make in
            make.left.equalToSuperview()
            make.centerY.equalToSuperview()
            make.width.equalTo(80.scale375())
            make.height.equalTo(48.scale375())
        }

        rttValueLabel.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalToSuperview()
            make.height.equalTo(24.scale375())
        }

        rttTitleLabel.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalTo(rttValueLabel.snp.bottom).offset(4.scale375())
            make.height.equalTo(20.scale375())
        }

        downLossView.snp.makeConstraints { make in
            make.left.equalTo(rttView.snp.right).offset(23.scale375())
            make.centerY.equalToSuperview()
            make.width.equalTo(80.scale375())
            make.height.equalTo(48.scale375())
        }

        downLossValueLabel.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalToSuperview()
            make.height.equalTo(24.scale375())
        }

        downLossTitleLabel.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalTo(downLossValueLabel.snp.bottom).offset(4.scale375())
            make.height.equalTo(20.scale375())
        }

        upLossView.snp.makeConstraints { make in
            make.left.equalTo(downLossView.snp.right).offset(23.scale375())
            make.centerY.equalToSuperview()
            make.width.equalTo(80.scale375())
            make.height.equalTo(48.scale375())
        }

        upLossValueLabel.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalToSuperview()
            make.height.equalTo(24.scale375())
        }

        upLossTitleLabel.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalTo(upLossValueLabel.snp.bottom).offset(4.scale375())
            make.height.equalTo(20.scale375())
        }
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private func bindInteraction() {
        isUserInteractionEnabled = true
        tableView.delegate = self
        tableView.dataSource = self
        guard let manager = manager else { return }

        manager.subscribe(StateSelector(keyPath: \NetWorkInfoState.downLoss))
            .receive(on: RunLoop.main)
            .sink { [weak self] downLoss in
                self?.onDownLossChanged(downLoss)
            }
            .store(in: &cancellables)

        manager.subscribe(StateSelector(keyPath: \NetWorkInfoState.rtt))
            .sink { [weak self] status in
                self?.onRttChanged(status)
            }
            .store(in: &cancellables)

        manager.subscribe(StateSelector(keyPath: \NetWorkInfoState.upLoss))
            .sink { [weak self] upLoss in
                self?.onUpLossChanged(upLoss)
            }
            .store(in: &cancellables)

        manager.subscribe(StateSelector(keyPath: \NetWorkInfoState.deviceTemperature))
            .sink { [weak self] temperature in
                self?.onTemperatureChanged(temperature)
            }
            .store(in: &cancellables)

        manager.subscribe(StateSelector(keyPath: \NetWorkInfoState.netWorkQuality))
            .sink { [weak self] netWorkQualit in
                self?.onNetWorkQualitChanged(netWorkQualit)
            }
            .store(in: &cancellables)
        manager.subscribe(StateSelector(keyPath: \NetWorkInfoState.audioState))
            .sink { [weak self] audioState in
                self?.onAudioStateChanged(audioState)
            }
            .store(in: &cancellables)
        manager
            .subscribe(StateSelector(keyPath: \NetWorkInfoState.audioQuality))
            .sink { [weak self] audioQuality in
                self?.onAudioQualityChanged(audioQuality)
            }
            .store(in: &cancellables)
        manager
            .subscribe(StateSelector(keyPath: \NetWorkInfoState.videoState))
            .sink { [weak self] videoState in
                self?.onVideoStateChanged(videoState)
            }
            .store(in: &cancellables)
        manager
            .subscribe(StateSelector(keyPath: \NetWorkInfoState.videoResolution))
            .sink { [weak self] videoResolution in
                self?.onVideoResolutionChanged(videoResolution: videoResolution)
            }
            .store(in: &cancellables)
        manager
            .subscribe(StateSelector(keyPath: \NetWorkInfoState.volume))
            .sink { [weak self] volume in
                self?.onVolumeChanged(volume)
            }
            .store(in: &cancellables)
        manager.kickedOutSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] in
                guard let self = self else { return }
                self.dismissPanel()
            }
            .store(in: &cancellables)
    }

    private func onDownLossChanged(_ downLoss: UInt32) {
        let color: UIColor
        switch downLoss {
            case 0..<5:
                color = .greenColor
            case 5..<10:
                color = .orangeColor
            default:
                color = .redColor
        }
        DispatchQueue.main.async { [weak self] in
            self?.downLossValueLabel.text = "\(downLoss)%"
            self?.downLossValueLabel.textColor = color
        }
    }

    private func onRttChanged(_ rtt: UInt32) {
        let color: UIColor
        switch rtt {
            case 0..<30:
                color = .greenColor
            case 30..<100:
                color = .orangeColor
            default:
                color = .redColor
        }
        DispatchQueue.main.async { [weak self] in
            self?.rttValueLabel.text = "\(rtt)ms"
            self?.rttValueLabel.textColor = color
        }
    }

    private func onUpLossChanged(_ upLoss: UInt32) {
        let color: UIColor
        switch upLoss {
            case 0..<5:
                color = .greenColor
            case 5..<10:
                color = .orangeColor
            default:
                color = .redColor
        }

        DispatchQueue.main.async { [weak self] in
            self?.upLossValueLabel.text = "\(upLoss)%"
            self?.upLossValueLabel.textColor = color
        }
    }

    private func onNetWorkQualitChanged(_ netWorkQualit: TUINetworkQuality) {
        let statusText: String
        let statusColor: UIColor
        let iconName: String
        switch netWorkQualit {
            case .excellent:
                statusText = .excellentText
                statusColor = .greenColor
                iconName = "live_networkinfo_wifi"
            case .good:
                statusText = .goodText
                statusColor = .greenColor
                iconName = "live_networkinfo_wifi"
            case .poor:
                statusText = .poorText
                statusColor = .greenColor
                iconName = "live_networkinfo_wifi_poor"
            case .bad:
                statusText = .badText
                statusColor = .redColor
                iconName = "live_networkinfo_wifi_bad"
            case .veryBad:
                statusText = .verybadText
                statusColor = .redColor
                iconName = "live_networkinfo_wifi_error"
            case .down:
                statusText = .downText
                statusColor = .redColor
                iconName = "live_networkinfo_wifi_error"
            default:
                return
        }

        DispatchQueue.main.async { [weak self] in
            if let index = self?.items.firstIndex(where: { $0 == .network }),
               let cell = self?.tableView.cellForRow(at: IndexPath(row: index, section: 0)) as? NetWorkInfoItemCell {
                cell.updateContent(
                    status: statusText,
                    iconName: iconName,
                    iconColor: statusColor
                )
            }

            self?.rttValueLabel.textColor = statusColor
            self?.downLossValueLabel.textColor = statusColor
            self?.upLossValueLabel.textColor = statusColor
        }
    }

    private func onTemperatureChanged(_ temperature: Int) {
        let statusColor: UIColor
        let statusText: String
        let iconName: String
        switch temperature {
            case 0:
                statusColor = .greenColor
                statusText = .normalText
                iconName = "live_networkinfo_temp"
            case 1:
                statusColor = .orangeColor
                statusText = .fairText
                iconName = "live_networkinfo_temp_warn"
            case 2, 3:
                statusColor = .redColor
                statusText = .seriousText
                iconName = "live_networkinfo_temp_error"
            default:
                statusColor = .greenColor
                statusText = .normalText
                iconName = "live_networkinfo_temp"
        }

        DispatchQueue.main.async { [weak self] in
            if let index = self?.items.firstIndex(where: { $0 == .temperature }),
               let cell = self?.tableView.cellForRow(at: IndexPath(row: index, section: 0)) as? NetWorkInfoItemCell {
                cell.updateContent(
                    status: statusText,
                    iconName: iconName,
                    iconColor: statusColor
                )
            }
        }
    }

    private func onAudioStateChanged(_ audioState: AudioState) {
        let statusColor: UIColor
        let statusText: String
        let detailText: String

        switch audioState {
            case .close,.mute:
                statusColor = .redColor
                statusText = .closeText
            case .normal:
                statusColor = .greenColor
                statusText = .normalText
            case .exception:
                statusColor = .redColor
                statusText = .exceptionText
        }
        DispatchQueue.main.async { [weak self] in
            if let index = self?.items.firstIndex(where: { $0 == .audio }),
               let cell = self?.tableView.cellForRow(at: IndexPath(row: index, section: 0)) as? NetWorkInfoItemCell {
                cell.updateContent(
                    status: statusText,
                    iconName: statusColor == .redColor ? "live_networkinfo_mic_error" : "live_networkinfo_mic",
                    iconColor: statusColor
                )
            }
        }
    }

    private func onVideoResolutionChanged(videoResolution: Int) {
        DispatchQueue.main.async { [weak self] in
            guard let index = self?.items.firstIndex(where: { $0 == .video }),
                  let cell = self?.tableView.cellForRow(at: IndexPath(row: index, section: 0)) as? NetWorkInfoItemCell else { return }
            
            let resolutionText = "\(videoResolution)P"            
            cell.updateContent(rightText: resolutionText)
        }
    }

    private func onVideoStateChanged(_ videoState: VideoState) {
        let statusColor: UIColor
        let statusText: String
        let detailText: String
        switch videoState {
            case .close:
                statusColor = .redColor
                statusText = .closeText
                detailText = .VideoDisable
            case .normal:
                statusColor = .greenColor
                statusText = .normalText
                detailText = .smoothStreaming
            case .exception:
                statusColor = .redColor
                statusText = .exceptionText
                detailText = .VideoException
        }
        
        DispatchQueue.main.async { [weak self] in
            if let index = self?.items.firstIndex(where: { $0 == .video }),
               let cell = self?.tableView.cellForRow(at: IndexPath(row: index, section: 0)) as? NetWorkInfoItemCell {
                cell.updateContent(
                    status: statusText,
                    detail: detailText,
                    iconName: statusColor == .redColor ? "live_networkinfo_video_error" : "live_networkinfo_video",
                    iconColor: statusColor
                )
            }
        }
    }

    private func onAudioQualityChanged(_ audioQuality: TUIAudioQuality) {
        DispatchQueue.main.async { [weak self] in
            guard let index = self?.items.firstIndex(where: { $0 == .audio }),
                  let cell = self?.tableView.cellForRow(at: IndexPath(row: index, section: 0)) as? NetWorkInfoItemCell else { return }
            
            let qualityText: String
            switch audioQuality {
                case .default:
                    qualityText = .AudioQualityDefault
                case .speech:
                    qualityText = .AudioQualitySpeech
                case .music:
                    qualityText = .AudioQualityMusic
                default:
                    return
            }
            
            cell.updateContent(rightText: qualityText)
        }
    }

    private func handleRightComponentsTap(for type: NetWorkInfoItemViewType) {
        if type == .audio {
            showAudioSettingsMenu()
        }
    }
    
    
    private func showAudioSettingsMenu() {
        let designConfig = ActionItemDesignConfig(titleColor: .whiteColor, backgroundColor: .blackColor, lineColor: .bgEntrycardColor)
        let items = [
            ActionItem(title: .AudioQualityDefault, designConfig: designConfig, actionClosure: { [weak self] _ in
                self?.manager?.onAudioQualityChanged(TUIAudioQuality.default)
                self?.dismissPanel()
            }),
            ActionItem(title: .AudioQualityMusic, designConfig: designConfig, actionClosure: { [weak self] _ in
                self?.manager?.onAudioQualityChanged(TUIAudioQuality.music)
                self?.dismissPanel()
            }),
            ActionItem(title: .AudioQualitySpeech, designConfig: designConfig, actionClosure: { [weak self] _ in
                self?.manager?.onAudioQualityChanged(TUIAudioQuality.speech)
                self?.dismissPanel()
            })
        ]
        
        let panelData = ActionPanelData(items: items, cancelText: .cancelText, cancelColor: .blackColor, cancelTitleColor: .whiteColor)
        let panel = ActionPanel(panelData: panelData)
        
        presentPanel(panel: panel)
    }
    
    private func presentPanel(panel: ActionPanel) {
        if let vc = WindowUtils.getCurrentWindowViewController() {
            popupViewController = vc
            let menuContainerView = MenuContainerView(contentView: panel)
            panel.cancelActionClosure = { [weak self] in
                guard let self = self else { return }
                dismissPanel()
            }
            menuContainerView.blackAreaClickClosure = dismissPanel
            let viewController = PopupViewController(
                contentView: menuContainerView,
                supportBlurView: false
            )
            popupViewController?.present(viewController, animated: true)
        }
    }

    private func dismissPanel() {
        if let presentedVC = popupViewController?.presentedViewController {
            presentedVC.dismiss(animated: true)
        }
    }



    private func onVolumeChanged(_ volume: Int) {
        DispatchQueue.main.async { [weak self] in
            if let index = self?.items.firstIndex(where: { $0 == .audio }),
               let cell = self?.tableView.cellForRow(at: IndexPath(row: index, section: 0)) as? NetWorkInfoItemCell {
                cell.updateSliderValue(Float(volume))
            }
        }
    }

    deinit {
        cancellables.forEach { $0.cancel() }
        cancellables.removeAll()
        manager = nil
        print("deinit \(type(of: self))")
    }

}

extension NetWorkInfoView: UITableViewDataSource, UITableViewDelegate {
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return items.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "NetWorkInfoItemCell", for: indexPath) as! NetWorkInfoItemCell
        let type = items[indexPath.row]
        let showDetail = (type == .video || type == .audio) && !isAudience
        cell.configure(with: type, showDetail: showDetail)
        
        cell.onRightComponentsTapped = { [weak self] in
            self?.handleRightComponentsTap(for: type)
        }
        
        if type == .audio {
            cell.onSliderValueChanged = { [weak self] value in
                self?.manager?.handleAudioSliderValueChanged(volume: Int(value))
            }
        }
        
        return cell
    }
    
    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        let type = items[indexPath.row]
        return type == .audio ? 90.scale375() : 66.scale375()
    }
}

fileprivate extension String {
    static let liveInfoTitle = internalLocalized("Live Information")
    static let roundTripDelay = internalLocalized("RTT")
    static let downlinkLoss = internalLocalized("Down Loss")
    static let uplinkLoss = internalLocalized("Up Loss")
    static let longSilence = internalLocalized("Long silence detected")
    static let audioClipping = internalLocalized("Audio clipping detected")
    static let audioInterruption = internalLocalized("Abnormal audio interruption detected")

    static let normalText = internalLocalized("normal")
    static let closeText = internalLocalized("close")
    static let exceptionText = internalLocalized("exception")

    static let excellentText = internalLocalized("excellent")
    static let goodText = internalLocalized("good")
    static let poorText = internalLocalized("poor")
    static let badText = internalLocalized("bad")
    static let verybadText = internalLocalized("veryBad")
    static let downText = internalLocalized("down")

    static let fairText = internalLocalized("fair")
    static let seriousText = internalLocalized("serious")

    static let AudioQualityDefault = internalLocalized("Default")
    static let AudioQualitySpeech = internalLocalized("Speech")
    static let AudioQualityMusic = internalLocalized("Music")

    static let smoothStreaming = internalLocalized("Smooth streaming")
    static let properVolume = internalLocalized("Proper volume ensures good viewing experience")
    static let regularChecks = internalLocalized("Regular checks ensure good viewing experience")
    static let avoidSwitching = internalLocalized("Avoid frequent network switching")
    static let VideoException = internalLocalized("Freezing streaming")
    static let VideoDisable = internalLocalized("Video capture disabled")
    static let cancelText = internalLocalized("Cancel")
}
