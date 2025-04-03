//
//  LSFeatureClickPanel.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/18.
//

import Foundation

enum LSFeatureItemType {
    case singleImage
    case imageAboveTitle
    case imageAboveTitleBottom
}

struct LSFeatureItemDesignConfig {
    var type: LSFeatureItemType = .singleImage
    var backgroundColor: UIColor? = .clear
    var titileColor: UIColor? = .g6
    var titleFont: UIFont = .customFont(ofSize: 10)
    var imageTopInset: CGFloat = 0.0
    var imageBottomInset: CGFloat = 0.0
    var imageLeadingInset: CGFloat = 0.0
    var imageTrailingInset: CGFloat = 0.0
    var imageSize: CGSize = CGSize(width: 24.scale375(), height: 24.scale375())
    var titleHeight: CGFloat = 17.scale375Height()
    var cornerRadius: CGFloat = 0
    var imageScale: CGFloat = 1.0
}

struct LSFeatureItem {
    var normalTitle: String?
    var normalImage: UIImage?
    var selectedTitle: String?
    var selectedImage: UIImage?
    var isSelected: Bool
    var isDisabled: Bool
    var designConfig: LSFeatureItemDesignConfig
    var actionClosure: ((LSFeatureItemButton)->Void)?
    
    init(normalTitle: String? = nil,
         normalImage: UIImage? = nil,
         selectedTitle: String? = nil,
         selectedImage: UIImage? = nil,
         isSelected: Bool = false,
         isDisabled: Bool = false,
         designConfig: LSFeatureItemDesignConfig = LSFeatureItemDesignConfig(),
         actionClosure: ((LSFeatureItemButton)->Void)? = nil) {
        self.normalTitle = normalTitle
        self.normalImage = normalImage
        self.selectedTitle = selectedTitle
        self.selectedImage = selectedImage
        self.isSelected = isSelected
        self.isDisabled = isDisabled
        self.designConfig = designConfig
        self.actionClosure = actionClosure
    }
}

class LSFeatureClickPanelModel {
    let id: UUID = UUID()
    var items: [LSFeatureItem] = []
    var itemDiff: CGFloat = 0.0
    var itemSize: CGSize = .zero
}

extension LSFeatureClickPanelModel: Equatable {
    static func == (lhs: LSFeatureClickPanelModel, rhs: LSFeatureClickPanelModel) -> Bool {
        return lhs.id == rhs.id
    }
}

class LSFeatureItemButton: UIControl {
    var item: LSFeatureItem
    init(item: LSFeatureItem) {
        self.item = item
        super.init(frame: .zero)
    }

    override var isSelected: Bool {
        didSet {
            item.isSelected = isSelected
            buttonTitle.text = item.isSelected ? item.selectedTitle : item.normalTitle
            buttonImageView.image = item.isSelected ? item.selectedImage : item.normalImage
        }
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    let buttonTitle: UILabel = {
        let label = UILabel()
        label.textAlignment = .center
        label.adjustsFontSizeToFitWidth = true
        label.isUserInteractionEnabled = false
        return label
    }()

    let buttonImageView: UIImageView = {
        let view = UIImageView()
        view.isUserInteractionEnabled = false
        return view
    }()

    let imageBgView: UIView = {
        let view = UIView()
        view.isHidden = true
        view.isUserInteractionEnabled = false
        return view
    }()

    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        setView()
    }

    func setView() {
        if item.designConfig.type == .imageAboveTitleBottom {
            imageBgView.backgroundColor = item.designConfig.backgroundColor
            imageBgView.layer.cornerRadius = item.designConfig.cornerRadius
            imageBgView.layer.masksToBounds = true
        } else {
            backgroundColor = item.designConfig.backgroundColor
            layer.cornerRadius = item.designConfig.cornerRadius
            layer.masksToBounds = true
        }

        addSubview(imageBgView)
        addSubview(buttonImageView)
        addSubview(buttonTitle)

        isSelected = item.isSelected
        buttonTitle.textColor = item.designConfig.titileColor
        buttonTitle.font = item.designConfig.titleFont
        self.alpha = item.isDisabled ? 0.5 : 1
        self.isEnabled = !item.isDisabled

        switch item.designConfig.type {
        case .singleImage:
            buttonImageView.snp.makeConstraints { make in
                make.width.equalTo(item.designConfig.imageSize.width)
                make.height.equalTo(item.designConfig.imageSize.height)
                make.top.bottom.equalToSuperview()
                make.left.right.equalToSuperview()
            }
        case .imageAboveTitle:
            buttonImageView.snp.makeConstraints { make in
                make.top.equalToSuperview().offset(item.designConfig.imageTopInset)
                make.width.equalTo(item.designConfig.imageSize.width)
                make.height.equalTo(item.designConfig.imageSize.height)
                make.centerX.equalToSuperview()
            }
            buttonTitle.snp.makeConstraints { make in
                make.top.equalTo(buttonImageView.snp.bottom).offset(2.scale375())
                make.width.equalToSuperview()
                make.height.equalTo(item.designConfig.titleHeight)
                make.bottom.equalToSuperview().offset(-2.scale375())
            }
        case .imageAboveTitleBottom:
            imageBgView.isHidden = false
            imageBgView.snp.makeConstraints { make in
                make.width.height.equalTo(56.scale375())
                make.top.left.right.equalToSuperview()
            }

            buttonImageView.snp.makeConstraints { make in
                make.width.height.equalTo(30.scale375())
                make.center.equalTo(imageBgView)
            }

            buttonTitle.snp.makeConstraints { make in
                make.top.equalTo(imageBgView.snp.bottom).offset(3.scale375())
                make.width.equalToSuperview()
                make.height.equalTo(17.scale375())
                make.bottom.equalToSuperview().offset(-2.scale375())
            }
        }
    }
}

class LSFeatureClickPanel: UIView {
    var model: LSFeatureClickPanelModel
    init(model: LSFeatureClickPanelModel) {
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
        setView()
    }

    func setView() {
        var leftButton: LSFeatureItemButton?
        for (index, item) in model.items.enumerated() {
            let button = createFeatureItemButton(item: item)
            button.snp.makeConstraints { make in
                if let leftBtn = leftButton {
                    make.leading.equalTo(leftBtn.snp.trailing).offset(model.itemDiff)
                } else {
                    make.leading.equalToSuperview()
                }
                make.width.equalTo(model.itemSize.width)
                make.top.equalToSuperview()
                make.bottom.equalToSuperview()
                if index == model.items.count - 1 {
                    make.trailing.equalToSuperview()
                }
            }
            leftButton = button
        }
    }

    func createFeatureItemButton(item: LSFeatureItem) -> LSFeatureItemButton {
        let view = LSFeatureItemButton(item: item)
        view.addTarget(self, action: #selector(itemClick(_:)), for: .touchUpInside)
        addSubview(view)
        return view
    }

    func updateFeatureItems(newItems: [LSFeatureItem]) {
        model.items = newItems
        for subview in subviews {
            subview.removeFromSuperview()
        }
        setView()
    }
}

// MARK: Action

extension LSFeatureClickPanel {
    @objc func itemClick(_ sender: LSFeatureItemButton) {
        sender.item.actionClosure?(sender)
    }
}
