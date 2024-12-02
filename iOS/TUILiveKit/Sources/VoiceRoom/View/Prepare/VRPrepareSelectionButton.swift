//
//  VRPrepareSelectionButton.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/11/18.
//

import Combine
import RTCCommon

class VRPrepareSelectionModel {
    var textLeftDiff:Float = 8.0
    var leftIcon: UIImage?
    @Published var midText: String = ""
    var rightIcon: UIImage?
}

class VRPrepareSelectionButton: UIButton {
    private var model: VRPrepareSelectionModel
    private var cancelableSet: Set<AnyCancellable> = []
    
    init(model: VRPrepareSelectionModel) {
        self.model = model
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        isViewReady = true
    }

    private lazy var leftIconImageView: UIImageView = {
        let view = UIImageView()
        view.image = self.model.leftIcon
        return view
    }()

    private lazy var contentLabel: UILabel = {
        let view = UILabel(frame: .zero)
        view.font = .customFont(ofSize: 14)
        view.textColor = .g7
        view.text = self.model.midText
        view.sizeToFit()
        return view
    }()

    private lazy var rightIconImageView: UIImageView = {
        let view = UIImageView()
        view.image = self.model.rightIcon
        return view
    }()
}

// MARK: - Layout
extension VRPrepareSelectionButton {
    private func constructViewHierarchy() {
        addSubview(leftIconImageView)
        addSubview(contentLabel)
        addSubview(rightIconImageView)
    }

    private func activateConstraints() {
        leftIconImageView.snp.remakeConstraints { make in
            make.leading.equalToSuperview()
            make.centerY.equalToSuperview()
            make.width.equalTo(16.scale375())
            make.height.equalTo(16.scale375())
        }

        contentLabel.snp.remakeConstraints { make in
            make.leading.equalTo(leftIconImageView.snp.trailing).offset(self.model.textLeftDiff)
            make.centerY.equalToSuperview()
            make.width.equalTo(contentLabel.mm_w)
            make.height.equalTo(contentLabel.mm_h)
        }

        rightIconImageView.snp.remakeConstraints { make in
            make.leading.equalTo(contentLabel.snp.trailing)
            make.centerY.equalToSuperview()
            make.width.equalTo(20.scale375())
            make.height.equalTo(20.scale375())
        }
    }

    func updateContentLabel(text: String) {
        contentLabel.text = text
        contentLabel.sizeToFit()
        contentLabel.snp.remakeConstraints { make in
            make.leading.equalTo(leftIconImageView.snp.trailing).offset(8)
            make.centerY.equalToSuperview()
            make.width.equalTo(contentLabel.mm_w)
            make.height.equalTo(contentLabel.mm_h)
        }
    }
}

// MARK: - Action
extension VRPrepareSelectionButton {
    private func bindInteraction() {
        model.$midText
            .receive(on: RunLoop.main)
            .sink { [weak self] text in
                guard let self = self else { return }
                self.updateContentLabel(text: text)
            }
            .store(in: &cancelableSet)
    }
}
