//
//  TRTCMeetingMoreViewVideoVC.swift
//  TRTCScenesDemo
//
//  Created by J J on 2020/5/15.
//  Copyright © 2022 Tencent. All rights reserved.
//

import TUIKitCommon
import UIKit

class TRTCLiveRoomBitrateTable: NSObject {
    var resolutionName: String = ""
    var resolution: Int = 0
    var defaultBitrate: Float = 0
    var minBitrate: Float = 0
    var maxBitrate: Float = 0
    var stepBitrate: Float = 0

    init(resolutionName: String, resolution: Int, defaultBitrate: Float, minBitrate: Float, maxBitrate: Float, stepBitrate: Float) {
        super.init()

        self.resolutionName = resolutionName
        self.resolution = resolution
        self.defaultBitrate = defaultBitrate
        self.minBitrate = minBitrate
        self.maxBitrate = maxBitrate
        self.stepBitrate = stepBitrate
    }
}

class TRTCLiveRoomMoreViewVideoVC: UIViewController, UIPickerViewDelegate, UIPickerViewDataSource {
    let TAG_RESOLUTION = 100
    let TAG_FPS = 200

    var resolutionTextField = UITextField()
    var fpsTextField = UITextField()

    lazy var resolutionLabel: UILabel = {
        let label = UILabel(frame: CGRect(x: 20, y: 30, width: 100, height: 25))
        label.textAlignment = NSTextAlignment.left
        label.text = .resolutionText
        label.font = UIFont.systemFont(ofSize: 18)
        label.textColor = .black
        return label
    }()

    lazy var resolutionImageView: UIImageView = {
        let imageView = UIImageView(image: UIImage(named: "live_back", in: liveRoomBundle(), compatibleWith: nil))
        imageView.transform = CGAffineTransform(rotationAngle: CGFloat(Double.pi))
        imageView.frame = CGRect(x: ScreenWidth - 20 - 16, y: 30 + (25 - 16) / 2, width: 16, height: 16)
        return imageView
    }()

    lazy var frameLabel: UILabel = {
        let label = UILabel(frame: CGRect(x: 20, y: 80, width: 100, height: 25))
        label.textAlignment = NSTextAlignment.left
        label.text = .framerateText
        label.font = UIFont.systemFont(ofSize: 18)
        label.textColor = .black
        return label
    }()

    lazy var frameImageView: UIImageView = {
        let imageView = UIImageView(image: UIImage(named: "live_back", in: liveRoomBundle(), compatibleWith: nil))
        imageView.transform = CGAffineTransform(rotationAngle: CGFloat(Double.pi))
        imageView.frame = CGRect(x: ScreenWidth - 20 - 16, y: 80 + (25 - 16) / 2, width: 16, height: 16)
        return imageView
    }()

    lazy var bitrateLabel: UILabel = {
        let label = UILabel(frame: CGRect(x: 20, y: 130, width: 100, height: 25))
        label.textAlignment = NSTextAlignment.left
        label.text = .bitrateText
        label.font = UIFont.systemFont(ofSize: 18)
        label.textColor = .black
        return label
    }()

    lazy var mirrorLabel: UILabel = {
        let label = UILabel(frame: CGRect(x: 20, y: 180, width: 100, height: 25))
        label.textAlignment = NSTextAlignment.left
        label.text = .localmirrorText
        label.font = UIFont.systemFont(ofSize: 18)
        label.textColor = .black
        return label
    }()

    let frameArray = ["15", "20", "24"]

    let bitrateTable = [TRTCLiveRoomBitrateTable](
        arrayLiteral:
        TRTCLiveRoomBitrateTable(resolutionName: "360 * 640", resolution: TRTCVideoResolution._640_360.rawValue,
                                 defaultBitrate: 900, minBitrate: 600, maxBitrate: 1200, stepBitrate: 10),
        TRTCLiveRoomBitrateTable(resolutionName: "540 * 960", resolution: TRTCVideoResolution._960_540.rawValue,
                                 defaultBitrate: 1300, minBitrate: 1000, maxBitrate: 1600, stepBitrate: 50),
        TRTCLiveRoomBitrateTable(resolutionName: "720 * 1280", resolution: TRTCVideoResolution._1280_720.rawValue,
                                 defaultBitrate: 1800, minBitrate: 1600, maxBitrate: 2100, stepBitrate: 50),
        TRTCLiveRoomBitrateTable(resolutionName: "1080 * 1920", resolution: TRTCVideoResolution._1920_1080.rawValue,
                                 defaultBitrate: 3500, minBitrate: 2100, maxBitrate: 3800, stepBitrate: 50)
    )
    var bitrateIndex = 3

    lazy var bitrateSlider: UISlider = {
        let slider = UISlider(frame: CGRect(x: UIScreen.main.bounds.size.width / 7.0 * 2.5 - 8, y: 126,
                                            width: UIScreen.main.bounds.size.width / 2.0 * 0.8, height: 30))

        let item = bitrateTable[bitrateIndex]
        slider.minimumValue = item.minBitrate / item.stepBitrate
        slider.maximumValue = item.maxBitrate / item.stepBitrate
        slider.value = item.defaultBitrate / item.stepBitrate
        slider.addTarget(self, action: #selector(bitrateSliderChanged), for: .valueChanged)
        slider.isContinuous = false

        return slider
    }()

    lazy var bitrateShowLabel: UILabel = {
        let label = UILabel(frame: CGRect(x: UIScreen.main.bounds.size.width / 7.0 * 5.5, y: 130,
                                          width: 100, height: 20))
        label.textAlignment = NSTextAlignment.left
        label.textColor = .black
        label.font = UIFont.systemFont(ofSize: 18)
        label.text = String(Int(bitrateTable[bitrateIndex].defaultBitrate)) + "kbps"
        return label
    }()

    private func getKeyWindow() -> UIWindow? {
        var keyWindow: UIWindow?
        for window in UIApplication.shared.windows {
            if window.isMember(of: UIWindow.self), window.isKeyWindow {
                keyWindow = window
                break
            }
        }
        return keyWindow
    }
    
    @objc func bitrateSliderChanged(_ slider: UISlider) {
        updateBitrate(bitrate: Int(slider.value * bitrateTable[bitrateIndex].stepBitrate))
    }

    var frameIndex = 2

    @objc func resolutionDidClick() {
        guard let window = getKeyWindow() else {
            return
        }
        let alert = TRTCLiveRoomResolutionAlert()
        alert.dataSource = bitrateTable
        alert.selectIndex = bitrateIndex
        window.addSubview(alert)
        alert.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        window.layoutIfNeeded()
        alert.show()
        alert.didSelectItem = { [weak self, weak alert] index in
            guard let `self` = self, let alert = alert else { return }
            self.updateResolution(index: index)
            alert.dismiss()
        }
    }

    @objc func frameDidClick() {
        guard let window = getKeyWindow() else {
            return
        }
        
        let dataSource = [
            TRTCLiveRoomBitrateTable(resolutionName: frameArray[0], resolution: 0,
                                     defaultBitrate: 0, minBitrate: 0, maxBitrate: 0, stepBitrate: 0),
            TRTCLiveRoomBitrateTable(resolutionName: frameArray[1], resolution: 0,
                                     defaultBitrate: 0, minBitrate: 0, maxBitrate: 0, stepBitrate: 0),
            TRTCLiveRoomBitrateTable(resolutionName: frameArray[2], resolution: 0,
                                     defaultBitrate: 0, minBitrate: 0, maxBitrate: 0, stepBitrate: 0),
        ]

        let alert = TRTCLiveRoomResolutionAlert()
        alert.titleLabel.text = liveRoomLocalize("Demo.TRTC.LiveRoom.framerate")
        alert.dataSource = dataSource
        alert.selectIndex = frameIndex
        window.addSubview(alert)
        alert.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        window.layoutIfNeeded()
        alert.show()
        alert.didSelectItem = { [weak self, weak alert] index in
            guard let `self` = self, let alert = alert else { return }
            self.updateFps(index: index)
            alert.dismiss()
        }
    }

    @objc func setPickViewAndTextField() {
        resolutionTextField = UITextField(frame: CGRect(x: UIScreen.main.bounds.size.width / 3.0, y: 30, width: UIScreen.main.bounds.size.width, height: 25))
        resolutionTextField.width = view.width
        resolutionTextField.backgroundColor = UIColor.red
        resolutionTextField.tintColor = .clear
        resolutionTextField.tag = TAG_RESOLUTION

        resolutionTextField.addGestureRecognizer(UITapGestureRecognizer(target: self, action: #selector(resolutionDidClick)))

        resolutionTextField.text = bitrateTable[bitrateIndex].resolutionName

        resolutionTextField.backgroundColor = .clear
        resolutionTextField.textAlignment = .left

        resolutionTextField.textColor = .black

        view.addSubview(resolutionTextField)

        let tap = UITapGestureRecognizer(target: self, action: #selector(hideKeyboard(tapG:)))
        tap.cancelsTouchesInView = false
        view.addGestureRecognizer(tap)

        fpsTextField = UITextField(frame: CGRect(x: UIScreen.main.bounds.size.width / 3.0, y: 80, width: UIScreen.main.bounds.size.width, height: 25))

        fpsTextField.tintColor = .clear
        fpsTextField.tag = TAG_FPS

        fpsTextField.addGestureRecognizer(UITapGestureRecognizer(target: self, action: #selector(frameDidClick)))

        fpsTextField.text = frameArray[2]

        fpsTextField.backgroundColor = .clear
        fpsTextField.textAlignment = .left

        fpsTextField.textColor = .black

        view.addSubview(fpsTextField)
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        view.addSubview(bitrateShowLabel)
        view.addSubview(resolutionLabel)
        view.addSubview(frameLabel)
        view.addSubview(bitrateLabel)
        view.addSubview(bitrateSlider)
        view.addSubview(resolutionImageView)
        view.addSubview(frameImageView)

        setPickViewAndTextField()
    }

    func numberOfComponents(in pickerView: UIPickerView) -> Int {
        return 1
    }

    func pickerView(_ pickerView: UIPickerView, numberOfRowsInComponent component: Int) -> Int {
        if pickerView.tag == TAG_RESOLUTION {
            return bitrateTable.count
        } else if pickerView.tag == TAG_FPS {
            return frameArray.count
        }
        return 1
    }

    func pickerView(_ pickerView: UIPickerView, titleForRow row: Int, forComponent component: Int) -> String? {
        if pickerView.tag == TAG_RESOLUTION {
            return bitrateTable[row].resolutionName
        } else if pickerView.tag == TAG_FPS {
            return frameArray[row]
        }
        return ""
    }

    func pickerView(_ pickerView: UIPickerView, didSelectRow row: Int, inComponent component: Int) {
        if pickerView.tag == TAG_RESOLUTION {
            print(String.resolutionSelectedText)
            updateResolution(index: row)

        } else if pickerView.tag == TAG_FPS {
            print(String.framerateSelectedText)
            updateFps(index: row)
        }
    }

    func updateResolution(index: Int) {
        bitrateIndex = index

        let item = bitrateTable[bitrateIndex]

        resolutionTextField.text = item.resolutionName
        let resolution = TRTCVideoResolution(rawValue: item.resolution)!
        TRTCLiveRoom.shareInstance().setVideo(resolution: resolution)

        bitrateSlider.minimumValue = item.minBitrate / item.stepBitrate
        bitrateSlider.maximumValue = item.maxBitrate / item.stepBitrate
        bitrateSlider.value = item.defaultBitrate / item.stepBitrate
        updateBitrate(bitrate: Int(item.defaultBitrate))
    }

    func updateFps(index: Int) {
        frameIndex = index
        fpsTextField.text = frameArray[index]
        TRTCLiveRoom.shareInstance().setVideo(fps: Int32(frameArray[index])!)
    }

    func updateBitrate(bitrate: Int) {
        bitrateShowLabel.text = String(bitrate) + "kbps"
        TRTCLiveRoom.shareInstance().setVideo(bitrate: Int32(bitrate))
    }

    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
    }

    @objc func hideKeyboard(tapG: UITapGestureRecognizer) {
        view.endEditing(true)
    }
}

// MARK: - internationalization string

fileprivate extension String {
    static var resolutionText: String {
        liveRoomLocalize("Demo.TRTC.LiveRoom.resolution")
    }
    static var framerateText: String {
        liveRoomLocalize("Demo.TRTC.LiveRoom.framerate")
    }
    static var bitrateText: String {
        liveRoomLocalize("Demo.TRTC.LiveRoom.bitrate")
    }
    static var localmirrorText: String {
        liveRoomLocalize("Demo.TRTC.LiveRoom.localmirror")
    }
    static var resolutionSelectedText: String {
        liveRoomLocalize("Demo.TRTC.LiveRoom.resolutionselected")
    }
    static var framerateSelectedText: String {
        liveRoomLocalize("Demo.TRTC.LiveRoom.framerateselected")
    }
}
