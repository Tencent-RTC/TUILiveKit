//
//  DropDownArrowButton.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/17.
//

import Foundation
import RTCCommon

class DropDownArrowModel {
    var diff:Float = 4.0
    let midText: Observable<String> = Observable("")
    var rightIcon: UIImage?
}

class DropDownArrowButton: UIButton {
    private var model: DropDownArrowModel

    init(model: DropDownArrowModel) {
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
        isViewReady = true
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
        model.midText.addObserver(self) { [weak self] _, _ in
            self?.updateLayout()
        }
    }

    private lazy var titleLab: UILabel = {
        let view = UILabel(frame: .zero)
        view.font = .customFont(ofSize: 14)
        view.textColor = .g7
        view.text = self.model.midText.value
        view.sizeToFit()
        view.isUserInteractionEnabled = false
        return view
    }()

    private lazy var rightIconImageView: UIImageView = {
        let view = UIImageView()
        view.image = self.model.rightIcon
        view.isUserInteractionEnabled = false
        return view
    }()
}

// MARK: Layout

extension DropDownArrowButton {
    func constructViewHierarchy() {
        addSubview(titleLab)
        addSubview(rightIconImageView)
    }

    func activateConstraints() {
        rightIconImageView.snp.remakeConstraints { make in
            make.trailing.equalToSuperview()
            make.centerY.equalToSuperview()
            make.width.equalTo(20.scale375())
            make.height.equalTo(20.scale375())
        }
        titleLab.snp.remakeConstraints { make in
            make.trailing.equalTo(rightIconImageView.snp.leading).offset(-self.model.diff)
            make.centerY.equalToSuperview()
            make.width.equalTo(titleLab.mm_w)
            make.height.equalTo(titleLab.mm_h)
        }
        self.mm_w = 20.scale375() + CGFloat(self.model.diff) + titleLab.mm_w
        self.snp.updateConstraints { make in
            make.width.equalTo(self.mm_w)
        }
    }

    func updateLayout() {
        titleLab.text = model.midText.value
        titleLab.sizeToFit()
        titleLab.snp.remakeConstraints { make in
            make.trailing.equalTo(rightIconImageView.snp.leading).offset(-self.model.diff)
            make.centerY.equalToSuperview()
            make.width.equalTo(titleLab.mm_w)
            make.height.equalTo(titleLab.mm_h)
        }
        self.mm_w = 20.scale375() + CGFloat(self.model.diff) + titleLab.mm_w
        self.snp.updateConstraints { make in
            make.width.equalTo(self.mm_w)
        }
    }
}
