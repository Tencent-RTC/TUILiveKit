//
//  String+Extension.swift
//  Pods
//
//  Created by ssc on 2025/8/29.
//

import Foundation

extension String {
    var localized: String {
        return atomicXBundle.localizedString(forKey: self, value: "", table: AtomicXLocalizeTableName)
    }

    //MARK: Replace String
    func localized(replace xxx_replace: String) -> String {
        return self.replacingOccurrences(of: "xxx", with: xxx_replace)
    }

    func localized(replace xxx_replace: String, yyy_replace: String) -> String {
        return self.localized(replace: xxx_replace).replacingOccurrences(of: "yyy", with: yyy_replace)
    }

    func localized(replace xxx_replace: String, yyy_replace: String, zzz_replace: String) -> String {
        return self.localized(replace: xxx_replace, yyy_replace: yyy_replace).replacingOccurrences(of: "zzz", with: zzz_replace)
    }

    func localized(replace xxx_replace: String, yyy_replace: String, zzz_replace: String, mmm_replace: String) -> String {
        return self.localized(replace: xxx_replace, yyy_replace: yyy_replace, zzz_replace: zzz_replace).replacingOccurrences(of: "mmm", with: mmm_replace)
    }

    func localized(replace xxx_replace: String, yyy_replace: String, zzz_replace: String, mmm_replace: String, nnn_replace: String) -> String {
        return self.localized(replace: xxx_replace, yyy_replace: yyy_replace, zzz_replace: zzz_replace, mmm_replace: mmm_replace).replacingOccurrences(of: "nnn", with: nnn_replace)
    }
}

let AtomicXLocalizeTableName = "AtomicXLocalized"
