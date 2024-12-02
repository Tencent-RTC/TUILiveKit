//
//  String+Extension.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/21.
//

import TUICore

extension String: SeatGridViewExtensionWrapper {}

extension SeatGridViewExtension where Base == String {
    public var localized: String {
        if let bundlePath = Bundle.sg_seatGridViewBundle.path(forResource: TUIGlobalization.getPreferredLanguage() ?? "", ofType: "lproj"),
           let bundle = Bundle(path: bundlePath) {
            
            return bundle.localizedString(forKey: base, value: "", table: "SeatGridViewLocalized")
        }
        return Bundle.sg_seatGridViewBundle.localizedString(forKey: base, value: "", table: "SeatGridViewLocalized")
    }
    
    func localizedReplace(replace: String) -> String {
        return base.replacingOccurrences(of: "xxx", with: replace)
    }
}
