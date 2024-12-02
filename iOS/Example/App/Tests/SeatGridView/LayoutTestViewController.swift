//
//  LayoutTestViewController.swift
//  TUILiveKitApp
//
//  Created by krabyu on 2024/10/17.
//

import UIKit
import SnapKit
import SeatGridView

public class LayoutTestViewController: UIViewController {
    
    var layoutModeList: [SGLayoutMode] = [.grid, .focus, .vertical, .free]
    var currentLayoutIndex: Int = 0
    
    var totalSeatCount: Int = 10
    var seatCountInRow: [Int] = [Int](repeating: 0, count: 4)
    var seatAlignmentInRow: [SGSeatViewLayoutRowAlignment] = [SGSeatViewLayoutRowAlignment](repeating: .center, count: 4)
    
    lazy var seatGridView: SeatGridView = {
        let view = SeatGridView()
        view.backgroundColor = .black.withAlphaComponent(0.8)
        return view
    }()
    
    let defaultLayoutLabel: UILabel = {
        let label = UILabel()
        label.text = localizeReplaceOneCharacter(origin: TUILiveKitAppLocalize("TUILiveKitApp.Test.SeatGridView.universalLayout.xxx"), xxx_replace: "Grid")
        label.textColor = .black
        label.font = UIFont.boldSystemFont(ofSize: 14)
        return label
    }()
    
    let seatCountLabel: UILabel = {
        let label = UILabel()
        label.text = TUILiveKitAppLocalize("TUILiveKitApp.Test.SeatGridView.seatCount")
        label.textColor = .black
        label.font = UIFont.boldSystemFont(ofSize: 14)
        return label
    }()
    
    let seatCountTextField: UITextField = {
        let textField = UITextField()
        textField.borderStyle = .roundedRect
        textField.placeholder = "10"
        textField.keyboardType = .numberPad
        return textField
    }()
    
    var defaultLayoutButtons: [UIButton] = []
    var buttonNames: [String] = ["Grid","Focus","Vertical"]
    var selectedButtonNames: [String] = ["Grid","Focus","Vertical"]
    var freeLayoutDefaultCount: [Int] = [1,3,3,5]
    var freeLayoutDefaultAlignmentIndex: [Int] = [5,3,4,0]
    
    let freeLayoutLabel: UILabel = {
        let label = UILabel()
        label.text = TUILiveKitAppLocalize("TUILiveKitApp.Test.SeatGridView.customLayout")
        label.textColor = .black
        label.font = UIFont.boldSystemFont(ofSize: 14)
        return label
    }()
    
    var freeLayoutInputViews: [FreeLayoutInputView] = (0..<4).map{_ in FreeLayoutInputView()}
    
    var seatRowConfigs: [SGSeatViewLayoutRowConfig] = []
    
    lazy var freeLayoutButton: UIButton = {
        let button = UIButton()
        button.setTitle("Free", for: .normal)
        button.backgroundColor = .blue.withAlphaComponent(0.3)
        button.setTitleColor(.black, for: .normal)
        button.addTarget(self, action: #selector(freeLayoutButtonClick), for: .touchUpInside)
        return button
    }()
    
    public override func viewDidLoad() {
        constructViewHierarchy()
        activateConstraints()
        
        seatGridView.setLayoutMode(layoutMode: layoutModeList[currentLayoutIndex], layoutConfig: SGSeatViewLayoutConfig())
    }
    
    private func constructViewHierarchy() {
        view.addSubview(seatGridView)
        view.addSubview(defaultLayoutLabel)
        view.addSubview(seatCountLabel)
        view.addSubview(seatCountTextField)
       
        for i in 0..<buttonNames.count {
            let button = UIButton()
            button.setTitle(buttonNames[i], for: .normal)
            button.setTitle(selectedButtonNames[i], for: .selected)
            button.backgroundColor = .blue.withAlphaComponent(0.3)
            button.tag = 1_000 + i
            button.setTitleColor(.black, for: .normal)
            button.addTarget(self, action: #selector(defaultLayoutButtonClick), for: .touchUpInside)
            
            defaultLayoutButtons.append(button)
            view.addSubview(button)
        }
        view.addSubview(freeLayoutLabel)
        
        for i in 0..<freeLayoutInputViews.count {
            view.addSubview(freeLayoutInputViews[i])
            freeLayoutInputViews[i].rowLabel.text = localizeReplaceOneCharacter(origin: TUILiveKitAppLocalize("TUILiveKitApp.Test.SeatGridView.line.xxx"),
                                                                                xxx_replace: "\(i+1)")
            freeLayoutInputViews[i].countTextFiled.text = "\(freeLayoutDefaultCount[i])"
            freeLayoutInputViews[i].spacingTextFiled.text = "10"
            freeLayoutInputViews[i].sizeTextFiled.text = "\(70)"
            freeLayoutInputViews[i].alignmentPicker.selectRow(freeLayoutDefaultAlignmentIndex[i], inComponent: 0, animated: false)
            
        }
        
        view.addSubview(freeLayoutButton)
    }
    
    private func activateConstraints() {
        seatGridView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(44 + 60)
            make.leading.equalToSuperview()
            make.trailing.equalToSuperview()
            make.height.equalTo(350)
        }
        defaultLayoutLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview()
            make.top.equalTo(seatGridView.snp.bottom).offset(20)
            make.width.equalToSuperview()
            make.height.equalTo(30)
        }
        seatCountLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview()
            make.top.equalTo(defaultLayoutLabel.snp.bottom).offset(5)
            make.width.equalTo(100)
            make.height.equalTo(30)
        }
        seatCountTextField.snp.makeConstraints { make in
            make.leading.equalTo(seatCountLabel.snp.trailing).offset(10)
            make.bottom.equalTo(seatCountLabel.snp.bottom)
            make.width.equalTo(100)
            make.height.equalTo(30)
        }
        
        let buttonWidth: CGFloat = 75
        let buttonHeight: CGFloat = 30
        let spacing: CGFloat = (UIScreen.main.bounds.width - CGFloat(defaultLayoutButtons.count) * buttonWidth) /
        CGFloat(defaultLayoutButtons.count + 1)
        var xOffset = 0.0
        for layoutButton in defaultLayoutButtons {
            layoutButton.snp.makeConstraints { make in
                make.leading.equalTo(xOffset)
                make.top.equalTo(seatCountTextField.snp.bottom).offset(10)
                make.width.equalTo(buttonWidth)
                make.height.equalTo(buttonHeight)
            }
            xOffset += buttonWidth + spacing
        }
        
        freeLayoutLabel.snp.makeConstraints { make in
            make.top.equalTo(defaultLayoutButtons[0].snp.bottom).offset(10)
            make.leading.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(30)
        }
        
        for i in 0..<freeLayoutInputViews.count {
            freeLayoutInputViews[i].snp.makeConstraints { make in
                make.leading.equalToSuperview()
                make.top.equalTo(freeLayoutLabel.snp.bottom).offset(40*i + 5)
                make.width.equalToSuperview()
                make.height.equalTo(40)
            }
        }
        
        freeLayoutButton.snp.makeConstraints { make in
            make.top.equalTo(freeLayoutInputViews[3].snp.bottom).offset(5)
            make.leading.equalToSuperview()
            make.width.equalTo(80)
            make.height.equalTo(30)
        }
        
    }
    
    @objc private func defaultLayoutButtonClick(_ sender: UIButton) {
        let layoutIndex = sender.tag - 1_000
        let layoutMode = layoutModeList[layoutIndex]
        var seatCount = 0
        if let countText = seatCountTextField.text, let count = Int(countText) {
            seatCount = count
        } else {
            seatCount = 10
        }
        seatGridView.setLayoutMode(layoutMode: layoutMode)
        var layoutTypeText = ""
        switch layoutMode {
            case .focus:
                layoutTypeText = "Focus"
            case .grid:
                layoutTypeText = "Grid"
            case .vertical:
                layoutTypeText = "Vertical"
            default:
                break
        }
        defaultLayoutLabel.text = localizeReplaceOneCharacter(origin: TUILiveKitAppLocalize("TUILiveKitApp.Test.SeatGridView.universalLayout.xxx"),
                                                              xxx_replace: layoutTypeText)
    }
    
    @objc private func freeLayoutButtonClick() {
        let freeLayoutConfig = getFreeLayoutConfig()
        seatGridView.setLayoutMode(layoutMode: .free, layoutConfig: freeLayoutConfig)
        defaultLayoutLabel.text = localizeReplaceOneCharacter(origin: TUILiveKitAppLocalize("TUILiveKitApp.Test.SeatGridView.universalLayout.xxx"),
                                                              xxx_replace: "")
    }
}


extension LayoutTestViewController {
    private func getFreeLayoutConfig() -> SGSeatViewLayoutConfig {
        var rowConfigs: [SGSeatViewLayoutRowConfig] = []
        let rowSpacing = 10.0
        
        for i in 0..<4 {
            guard let countText = freeLayoutInputViews[i].countTextFiled.text, let count = Int(countText) else {
                continue
            }
            
            guard let spacingText = freeLayoutInputViews[i].spacingTextFiled.text, let spacing = Float(spacingText) else {
                continue
            }
            
            guard let sizeText = freeLayoutInputViews[i].sizeTextFiled.text, let size = Float(sizeText) else {
                continue
            }
            
            guard let alignment = SGSeatViewLayoutRowAlignment(rawValue: freeLayoutInputViews[i].alignmentPicker.selectedRow(inComponent: 0)) else {
                continue
            }
            
            let rowConfig = SGSeatViewLayoutRowConfig(count: count,
                                                        seatSpacing: CGFloat(spacing),
                                                        seatSize: CGSize(width: Double(size), height: Double(size)),
                                                        alignment: alignment)
            rowConfigs.append(rowConfig)
        }
        
        return SGSeatViewLayoutConfig(rowConfigs: rowConfigs, rowSpacing: rowSpacing)
    }
}


class FreeLayoutInputView: UIView, UIPickerViewDelegate, UIPickerViewDataSource{
    let rowLabel: UILabel = {
        let label = UILabel()
        label.textColor = .black
        label.font = UIFont.systemFont(ofSize: 12)
        return label
    }()
    
    let countTipsLabel: UILabel = {
        let label = UILabel()
        label.text = "count:"
        label.textColor = .black
        label.font = UIFont.systemFont(ofSize: 12)
        return label
    }()
    
    let countTextFiled: UITextField = {
        let textField = UITextField()
        textField.borderStyle = .roundedRect
        textField.placeholder = "2"
        textField.keyboardType = .numberPad
        return textField
    }()
    
    let spacingTipsLabel: UILabel = {
        let label = UILabel()
        label.text = "spacing:"
        label.textColor = .black
        label.font = UIFont.systemFont(ofSize: 12)
        return label
    }()
    
    let spacingTextFiled: UITextField = {
        let textField = UITextField()
        textField.borderStyle = .roundedRect
        textField.placeholder = "10"
        textField.keyboardType = .numberPad
        return textField
    }()
    
    let sizeTipsLabel: UILabel = {
        let label = UILabel()
        label.text = "size:"
        label.textColor = .black
        label.font = UIFont.systemFont(ofSize: 12)
        return label
    }()
    
    let sizeTextFiled: UITextField = {
        let textField = UITextField()
        textField.borderStyle = .roundedRect
        textField.placeholder = "70"
        textField.keyboardType = .numberPad
        return textField
    }()
    
    let alignmentTipsLabel: UILabel = {
        let label = UILabel()
        label.text = "alignment:"
        label.textColor = .black
        label.font = UIFont.systemFont(ofSize: 12)
        return label
    }()
    
    lazy var alignmentPicker: UIPickerView = {
        let pickerView = UIPickerView()
        pickerView.delegate = self
        pickerView.dataSource = self
        return pickerView
    }()
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }
    
    private func constructViewHierarchy() {
        addSubview(rowLabel)
        addSubview(countTipsLabel)
        addSubview(countTextFiled)
        addSubview(spacingTipsLabel)
        addSubview(spacingTextFiled)
        addSubview(sizeTipsLabel)
        addSubview(sizeTextFiled)
        addSubview(alignmentTipsLabel)
        addSubview(alignmentPicker)
    }
    
    private func activateConstraints() {
        rowLabel.snp.makeConstraints { make in
            make.top.leading.equalToSuperview()
            make.height.equalTo(30)
            make.width.equalTo(35)
        }
        countTipsLabel.snp.makeConstraints { make in
            make.top.equalToSuperview()
            make.leading.equalTo(rowLabel.snp.trailing)
            make.height.equalTo(30)
            make.width.equalTo(40)
        }
        countTextFiled.snp.makeConstraints { make in
            make.top.equalToSuperview()
            make.leading.equalTo(countTipsLabel.snp.trailing)
            make.height.equalTo(30)
            make.width.equalTo(35)
        }
        sizeTipsLabel.snp.makeConstraints { make in
            make.top.equalToSuperview()
            make.leading.equalTo(countTextFiled.snp.trailing).offset(5)
            make.height.equalTo(30)
            make.width.equalTo(30)
        }
        sizeTextFiled.snp.makeConstraints { make in
            make.top.equalToSuperview()
            make.leading.equalTo(sizeTipsLabel.snp.trailing)
            make.height.equalTo(30)
            make.width.equalTo(35)
        }
        alignmentTipsLabel.snp.makeConstraints { make in
            make.top.equalToSuperview()
            make.leading.equalTo(sizeTextFiled.snp.trailing).offset(5)
            make.height.equalTo(30)
            make.width.equalTo(60)
        }
        alignmentPicker.snp.makeConstraints { make in
            make.top.equalToSuperview()
            make.leading.equalTo(alignmentTipsLabel.snp.trailing).offset(-5)
            make.height.equalTo(40)
            make.width.equalTo(170)
        }
    }
    
    func numberOfComponents(in pickerView: UIPickerView) -> Int {
        return 1
    }

    func pickerView(_ pickerView: UIPickerView, numberOfRowsInComponent component: Int) -> Int {
        return 6
    }

    func pickerView(_ pickerView: UIPickerView, attributedTitleForRow row: Int, forComponent component: Int) -> NSAttributedString? {
        var title = ""
        switch row {
            case 0:
                title =  "spaceAround"
            case 1:
                title = "spaceBetween"
            case 2:
                title = "spaceEvenly"
            case 3:
                title = "start"
            case 4:
                title = "end"
            case 5:
                title = "center"
            default:
                title = "center"
        }
        let attributedString = NSAttributedString(string: title, 
                                                  attributes: [NSAttributedString.Key.font: UIFont.systemFont(ofSize: 8),
                                                               NSAttributedString.Key.foregroundColor: UIColor.black,])
        return attributedString
    }
}
