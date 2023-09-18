//
//  AnchorPKPanel.swift
//  TRTCScenesDemo
//
//  Created by xcoderliu on 3/13/20.
//  Copyright © 2020 xcoderliu. All rights reserved.
//

import Foundation
import TUIKitCommon
import SnapKit
import Kingfisher
import TUICore

@objcMembers
public class AnchorPKCell: UITableViewCell {
    public lazy var coverImg: UIImageView = {
        let img = UIImageView()
        img.layer.cornerRadius = 20
        img.layer.masksToBounds = true
        return img
    }()
    
    public lazy var inviteLabel: UILabel = {
        let label = UILabel()
        label.text = .inviteText
        label.textAlignment = NSTextAlignment.center
        label.isUserInteractionEnabled = true
        label.textColor = .white
        label.backgroundColor = UIColor(hex: "29CC85")
        label.clipsToBounds = true
        return label
    }()
    
    public lazy var infoLabel: UILabel = {
        let label = UILabel()
        label.textColor = .black
        label.font = UIFont.systemFont(ofSize: 13)
        return label
    }()
    
    public override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        backgroundColor = .clear
        selectionStyle = .none
    }
    
    public required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    public override func draw(_ rect: CGRect) {
        super.draw(rect)
        inviteLabel.layer.cornerRadius = inviteLabel.frame.height*0.5
    }
    
    public func config(model: TRTCLiveRoomInfo) {
        
        self.addSubview(coverImg)
        coverImg.snp.remakeConstraints { (make) in
            make.top.leading.equalTo(10)
            make.left.equalTo(20)
            make.bottom.equalTo(-10)
            make.width.equalTo(40)
        }
        
        self.addSubview(inviteLabel)
        inviteLabel.snp.remakeConstraints { (make) in
            make.top.equalTo(15)
            make.bottom.equalTo(-15)
            make.right.equalTo(-20)
            make.width.equalTo(75)
        }
        
        if let url = URL.init(string: model.coverUrl.count > 0 ? model.coverUrl : sdWebImgPlaceHolderStr()) {
            coverImg.kf.setImage(with: .network(url))
        }
        
        self.addSubview(infoLabel)
        infoLabel.snp.remakeConstraints { (make) in
            make.leading.equalTo(coverImg.snp.trailing).offset(5)
            make.top.equalTo(5)
            make.bottom.trailing.equalTo(-5)
        }
        infoLabel.text = "\(model.ownerName)\n\(model.roomName)"
        infoLabel.font = UIFont.systemFont(ofSize: 15)
        infoLabel.numberOfLines = 2
    }
}

@objcMembers
public class AnchorPKPanel: UIView, UITableViewDelegate,
                            UITableViewDataSource {
    public weak var liveRoom: TRTCLiveRoom? = nil
    public var isLoading: Bool = false
    public var roomInfos: [TRTCLiveRoomInfo] = []
    public var pkWithRoom: ((TRTCLiveRoomInfo)->Void)? = nil
    public var shouldHidden: (()->Void)? = nil
    public lazy var anchorTable: UITableView = {
        let table = UITableView(frame: .zero, style: .plain)
        table.register(AnchorPKCell.classForCoder(), forCellReuseIdentifier: "AnchorPKCell")
        table.separatorColor = UIColor.clear
        table.backgroundColor = .white
        table.allowsSelection = true
        return table
    }()
    
    public lazy var roomInputTextField: UITextField = {
        let textField = UITextField.init(frame: .zero)
        textField.backgroundColor = UIColor(0xd8d8d8)
        textField.layer.cornerRadius = 8
        let leftLabel = UILabel.init(frame: CGRect.init(x: 0, y: 0, width: 120, height: 40))
        leftLabel.text = .roomNumberText
        leftLabel.font = UIFont.systemFont(ofSize: 15)
        leftLabel.textColor = .black
        textField.leftView = leftLabel
        textField.leftViewMode = .always
        
        let attributeString = NSMutableAttributedString.init(string: .roomNumberText)
        attributeString.addAttributes([.foregroundColor : UIColor.gray, .font : UIFont.systemFont(ofSize:
         15)], range: NSRange.init(location: 0, length: attributeString.length))
        textField.attributedPlaceholder = attributeString
        textField.keyboardType = .numberPad
        textField.textColor = .black
        return textField
    }()
    
    public lazy var invitePKBtn: UIButton = {
        let btn = UIButton.init(frame: .zero)
        btn.setTitle(.invitePKText, for: .normal)
        btn.setTitleColor(.white, for: .normal)
        btn.setBackgroundImage(UIColor.init(0xd8d8d8).trans2Image(), for: .normal)
        btn.setBackgroundImage(UIColor.systemBlue.trans2Image(), for: .selected)
        btn.layer.cornerRadius = 8
        btn.layer.masksToBounds = true
        return btn
    }()
    
    public override init(frame: CGRect) {
        super.init(frame: frame)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardFrameChange(noti:)),
         name: UIResponder.keyboardWillChangeFrameNotification, object: nil)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        NotificationCenter.default.removeObserver(self)
    }
    
    private var isViewReady = false
    public override func didMoveToSuperview() {
        super.didMoveToSuperview()
        guard !isViewReady else {
            return
        }
        isViewReady = true
        backgroundColor = .white
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
    }
    
    public func endLoading() {
#if RTCube_APPSTORE
        isLoading = false
        anchorTable.reloadData()
#endif
    }
    
    public func loadRoomsInfo() {
#if RTCube_APPSTORE
        roomInfos = []
        anchorTable.reloadData()
        isLoading = true
        TUILiveRoomProfileManager.sharedManager().getRoomList { [weak self] (ids) in
            guard let self = self else { return }
            if ids.count == 0 {
                self.endLoading()
                return
            }
            self.liveRoom?.getRoomInfos(roomIDs: ids, callback: { [weak self] (code, error, infos) in
                guard let self = self else { return }
                DispatchQueue.main.async { [weak self] in
                    guard let self = self else { return }
                    self.roomInfos = infos.filter {
                        $0.ownerId != (TUILogin.getUserID() ?? "")
                    }
                    self.endLoading()
                }
            })
        } failed: { [weak self] (code, error) in
            guard let self = self else { return }
            self.endLoading()
            debugPrint(error)
        }
#endif
    }
    
    public func hiddenPanel() {
        isHidden = true
        if let hidden = shouldHidden {
            hidden()
        }
        endEditing(true)
    }
    
    public override func point(inside point: CGPoint, with event: UIEvent?) -> Bool {
        if !isHidden, !bounds.contains(point) {
            if roomInputTextField.isFirstResponder {
                roomInputTextField.resignFirstResponder()
            } else {
                hiddenPanel()
            }
        }
        return super.point(inside: point, with: event)
    }
}

// MARK: - UI Layout
extension AnchorPKPanel {
    
    private func constructViewHierarchy() {
#if RTCube_APPSTORE
        addSubview(anchorTable)
#else
        addSubview(roomInputTextField)
        addSubview(invitePKBtn)
#endif
    }
    
    private func activateConstraints() {
#if RTCube_APPSTORE
        anchorTable.snp.remakeConstraints { (make) in
            make.leading.top.width.equalTo(self)
            make.height.equalTo(self)
        }
#else
        roomInputTextField.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview().inset(20)
            make.top.equalTo(20)
            make.height.equalTo(48)
        }
        invitePKBtn.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview().inset(40)
            make.height.equalTo(48)
            make.top.equalTo(roomInputTextField.snp.bottom).offset(40)
        }
#endif
    }
    
    private func bindInteraction() {
#if RTCube_APPSTORE
        anchorTable.delegate = self
        anchorTable.dataSource = self
#else
        roomInputTextField.addTarget(self, action: #selector(roomNumberTextFieldValueChange(sender:)), for: .editingChanged)
        invitePKBtn.addTarget(self, action: #selector(clickInvitePK), for: .touchUpInside)
#endif
    }
    
}

// MARK: - Touch Event
extension AnchorPKPanel {
    
    @objc private func clickInvitePK() {
        guard let roomId = roomInputTextField.text, !roomId.isEmpty else {
            return
        }
        liveRoom?.getRoomInfos(roomIDs: [roomId], callback: { [weak self] (code, msg, rooms) in
            guard let self = self else { return }
            if let roomInfo = rooms.first {
                self.pkWithRoom?(roomInfo)
                self.hiddenPanel()
            } else {
                self.endEditing(true)
                self.makeToast("room cannot found")
            }
        })
    }

    @objc private func roomNumberTextFieldValueChange(sender: UITextField) {
        if let text = sender.text, text.count > 0 {
            invitePKBtn.isSelected = true
            invitePKBtn.isUserInteractionEnabled = true
        } else {
            invitePKBtn.isSelected = false
            invitePKBtn.isUserInteractionEnabled = false
        }
    }
}

// MARK: - KeyboardFrame Changed
extension AnchorPKPanel {
    @objc
    func keyboardFrameChange(noti : Notification) {
        guard let superview = superview else{ return }
        guard !isHidden else {
            return
        }
        guard let info = noti.userInfo else {
            return
        }
        guard let rect = info[UIResponder.keyboardFrameEndUserInfoKey] as? CGRect else {
            return
        }
        if rect.minY == SCREEN_HEIGHT {
            transform = .identity
        } else {
            transform = CGAffineTransform(translationX: 0, y: -(superview.height - invitePKBtn.mm_maxY) + rect.minY)
        }
    }

}

// MARK: - UITableViewDataSource
extension AnchorPKPanel {
    
    public func numberOfSections(in tableView: UITableView) -> Int {
        return 1
    }
    
    public func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return roomInfos.count
    }
    
    public func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "AnchorPKCell",for: indexPath)
        if let pkcell = cell as? AnchorPKCell {
            if indexPath.row < roomInfos.count {
                let room = roomInfos[indexPath.row]
                pkcell.config(model: room)
            }
        }
        return cell
    }
}

// MARK: - UITableViewDataSource
extension AnchorPKPanel {
    public func tableView(_ tableView: UITableView, heightForHeaderInSection section: Int) -> CGFloat {
        return 40
    }
    
    public func tableView(_ tableView: UITableView, willDisplayHeaderView view: UIView, forSection section: Int) {
        view.tintColor = .clear
        if let header = view as? UITableViewHeaderFooterView {
            header.textLabel?.textColor = .white
        }
    }
    
    public func tableView(_ tableView: UITableView, titleForHeaderInSection section: Int) -> String? {
        if roomInfos.count > 0 {
            return " "
        }
        return isLoading ? .loadingText : .noAnchorText
    }
    
    public func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return 60
    }
    
    public func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        tableView.deselectRow(at: indexPath, animated: false)
        if indexPath.row < roomInfos.count {
            if let pk = pkWithRoom {
                let room = roomInfos[indexPath.row]
                pk(room)
                isHidden = true
                if let hidden = shouldHidden {
                    hidden()
                }
            }
        }
    }
    
    public func tableView(_ tableView: UITableView, viewForHeaderInSection section: Int) -> UIView? {
        let headerLabel = UILabel()
        headerLabel.frame = CGRect(x: 0, y: 40, width: tableView.bounds.size.width, height: 50)
        headerLabel.textAlignment = NSTextAlignment.center
        headerLabel.text = .invitePKText
        headerLabel.textColor = .black
        headerLabel.backgroundColor = .clear
        headerLabel.isUserInteractionEnabled = true
        
        let cancel = UIButton(frame: CGRect(x: self.bounds.size.width * 4.0 / 5.0, y: 0, width: self.bounds.size.width / 5.0, height: 40))
        cancel.setTitle(.cancelText, for: UIControl.State.normal)
        cancel.backgroundColor = .clear
        cancel.addTarget(self, action: #selector(hiddenPanel), for: UIControl.Event.touchUpInside)
        cancel.setTitleColor(.black, for: .normal)
        headerLabel.addSubview(cancel)
        return headerLabel
    }
}

// MARK: - internationalization string
fileprivate extension String {
    static var inviteText: String {
        liveRoomLocalize("Demo.TRTC.LiveRoom.invite")
    }
    static var invitePKText: String {
        liveRoomLocalize("Demo.TRTC.LiveRoom.invitepk")
    }
    static var loadingText: String {
        liveRoomLocalize("Demo.TRTC.LiveRoom.loading")
    }
    static var noAnchorText: String {
        liveRoomLocalize("Demo.TRTC.LiveRoom.noanchor")
    }
    static var cancelText: String {
        liveRoomLocalize("Demo.TRTC.LiveRoom.cancel")
    }
    static var roomNumberText: String {
        liveRoomLocalize("Demo.TRTC.LiveRoom.roomNumber")
    }

}
