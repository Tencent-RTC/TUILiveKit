//
//  AnchorPKPanel.swift
//  TRTCScenesDemo
//
//  Created by xcoderliu on 3/13/20.
//  Copyright © 2020 xcoderliu. All rights reserved.
//

import Foundation
import TXAppBasic
import SnapKit
import Kingfisher

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
        table.delegate = self
        table.dataSource = self
        table.separatorColor = UIColor.clear
        table.allowsSelection = true
        return table
    }()
    
    public override func didMoveToSuperview() {
        anchorTable.backgroundColor = .white
        self.addSubview(anchorTable)
        anchorTable.snp.remakeConstraints { (make) in
            make.leading.top.width.equalTo(self)
            make.height.equalTo(self)
        }
    }
    
    public func endLoading() {
        isLoading = false
        anchorTable.reloadData()
    }
    
    public func loadRoomsInfo() {
        roomInfos = []
        anchorTable.reloadData()
        isLoading = true
        TUILiveRoomProfileManager.sharedManager().getRoomList { [weak self] (ids) in
            guard let `self` = self else { return }
            let uintIDs = ids.compactMap {
                Int($0)
            }
            if uintIDs.count == 0 {
                self.endLoading()
                return
            }
            let numberIds = uintIDs.map { (value) -> NSNumber in
                return NSNumber.init(value: value)
            }
            self.liveRoom?.getRoomInfos(roomIDs: numberIds, callback: { (code, error, infos) in
                DispatchQueue.main.async {
                    self.roomInfos = infos.filter {
                        $0.ownerId != V2TIMManager.sharedInstance()?.getLoginUser() ?? ""
                    }
                    self.endLoading()
                }
            })
        } failed: { [weak self] code, error in
            guard let `self` = self else { return }
            self.endLoading()
            debugPrint(error)
        }
    }
    
    //MARK: - tableview dataSource
    
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
    
    public func numberOfSections(in tableView: UITableView) -> Int {
        return 1
    }
    
    public func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return roomInfos.count
    }
    
    public func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "AnchorPKCell",for: indexPath)
        if let pkcell = cell as?AnchorPKCell {
            if indexPath.row < roomInfos.count {
                let room = roomInfos[indexPath.row]
                pkcell.config(model: room)
            } else {
                pkcell.config(model: TRTCLiveRoomInfo.init(roomId: "", roomName: "",
                                                           coverUrl: "", ownerId: "",
                                                           ownerName: "", streamUrl: "",
                                                           memberCount: 0, roomStatus: .none))
            }
        }
        
        
        return cell
    }
    
    public func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return 60
    }
    
    //MARK: - delegate
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
    
    public func hiddenPanel() {
        isHidden = true
        if let hidden = shouldHidden {
            hidden()
        }
    }
    
    public override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        if let touch = event?.allTouches?.first {
            let loc:CGPoint = touch.location(in: touch.view)
            //insert your touch based code here
            if loc.y > 288 {
                isHidden = true
                if let hidden = shouldHidden {
                    hidden()
                }
            }
        }
    }
}

private extension String {
    static let inviteText = LiveRoomLocalize("Demo.TRTC.LiveRoom.invite")
    static let invitePKText = LiveRoomLocalize("Demo.TRTC.LiveRoom.invitepk")
    static let loadingText = LiveRoomLocalize("Demo.TRTC.LiveRoom.loading")
    static let noAnchorText = LiveRoomLocalize("Demo.TRTC.LiveRoom.noanchor")
    static let cancelText = LiveRoomLocalize("Demo.TRTC.LiveRoom.cancel")
}
