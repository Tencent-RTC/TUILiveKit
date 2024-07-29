//
//  AnchorVideoParametersSettingPanel.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/15.
//

import Foundation
import RTCRoomEngine
import Combine
import RTCCommon

class AnchorVideoParametersSettingPanel: UIView {
    private let store: LiveStore
    private let routerStore: RouterStore
    private var cancellableSet = Set<AnyCancellable>()
    weak var rootController: UIViewController?
    init(store: LiveStore, routerStore: RouterStore) {
        self.store = store
        self.routerStore = routerStore
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()

    private var isViewReady: Bool = false
    override func layoutSubviews() {
        super.layoutSubviews()
        guard !isViewReady else { return }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
        setView()
    }

    private lazy var backButton: UIButton = {
        let view = UIButton(type: .system)
        view.setBackgroundImage(.liveBundleImage("live_back_icon"), for: .normal)
        view.addTarget(self, action: #selector(backButtonClick), for: .touchUpInside)
        return view
    }()

    private let titleLabel: UILabel = {
        let view = UILabel()
        view.text = .settingTitleText
        view.textColor = .g7
        view.font = .customFont(ofSize: 16)
        view.textAlignment = .center
        return view
    }()

    private lazy var videoQualitySelectionModel: DropDownArrowModel = {
        let model = DropDownArrowModel()
        model.midText.value = store.selectCurrent(MediaSelectors.getVideoQuality).getString()
        model.rightIcon = .liveBundleImage("live_drop_down_arrow")
        store.select(MediaSelectors.getVideoQuality)
            .receive(on: RunLoop.main)
            .sink { videoQuality in
                self.videoQualitySelectionModel.midText.value = videoQuality.getString()
            }
            .store(in: &cancellableSet)
        return model
    }()

    private let videoQualityTitleLabel: UILabel = {
        let view = UILabel()
        view.text = .sharpnessText
        view.textColor = .g5
        view.font = .customFont(ofSize: 16)
        view.textAlignment = .left
        return view
    }()

    private lazy var videoQualityButton: DropDownArrowButton = {
        let view = DropDownArrowButton(model: videoQualitySelectionModel)
        view.addTarget(self, action: #selector(videoQualityButtonClick), for: .touchUpInside)
        return view
    }()

    private func setView() {
        backgroundColor = .g2
        layer.cornerRadius = 20
        layer.masksToBounds = true
    }

}

// MARK: Layout

extension AnchorVideoParametersSettingPanel {
    func constructViewHierarchy() {
        addSubview(backButton)
        addSubview(titleLabel)
        addSubview(videoQualityTitleLabel)
        addSubview(videoQualityButton)
    }

    func activateConstraints() {
        snp.remakeConstraints { make in
            if isPortrait {
                make.height.equalTo(350.scale375Height())
            } else {
                make.width.equalTo(375.scale375())
            }
            make.edges.equalToSuperview()
        }

        backButton.snp.remakeConstraints { make in
            make.leading.equalToSuperview().inset(20)
            make.top.equalToSuperview().inset(20)
            make.height.equalTo(24.scale375())
            make.width.equalTo(24.scale375())
        }

        titleLabel.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(20.scale375Height())
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(24.scale375Height())
        }

        videoQualityButton.snp.makeConstraints { make in
            make.top.equalTo(titleLabel.snp.bottom).offset(32.scale375Height())
            make.trailing.equalToSuperview().inset(24)
            make.height.equalTo(20.scale375Height())
        }

        videoQualityTitleLabel.snp.makeConstraints { make in
            make.top.centerY.equalTo(videoQualityButton)
            make.leading.equalToSuperview().inset(24)
            make.height.equalTo(22.scale375Height())
        }
    }
}

// MARK: Action

extension AnchorVideoParametersSettingPanel {
    @objc func backButtonClick(sender: UIButton) {
        routerStore.router(action: .dismiss)
    }

    @objc func videoQualityButtonClick() {
        var items: [ActionItem] = []
        let designConfig = ActionItemDesignConfig()
        var config: ActionItemDesignConfig
        let categoryList: [TUIVideoQuality] = [.quality360P, .quality540P, .quality720P, .quality1080P]
        for category in categoryList {
            if category == categoryList.last {
                config = ActionItemDesignConfig(lineWidth: 7)
            } else {
                config = designConfig
            }
            let item = ActionItem(title: category.getString(), designConfig: config, actionClosure: { [weak self] _ in
                guard let self = self else { return }
                self.store.dispatch(action: MediaActions.updateVideoQuality(payload: category))
            })
            items.append(item)
        }
        routerStore.router(action: .present(.listMenu(items)))
    }
}

private extension String {
    static var settingTitleText: String {
        localized("live.anchor.setting.video.parameters")
    }

    static var sharpnessText: String {
        localized("live.anchor.setting.video.sharpness")
    }
}
