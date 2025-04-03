enum PrivacyStatus {
  publicity(0),
  privacy(1);

  final int value;

  const PrivacyStatus(this.value);

  static PrivacyStatus fromInt(int value) {
    return PrivacyStatus.values.firstWhere(
      (e) => e.value == value,
      orElse: () => PrivacyStatus.publicity,
    );
  }
}

enum LinkStatus {
  none(0),
  applying(1),
  linking(2);

  final int value;

  const LinkStatus(this.value);

  static LinkStatus fromInt(int value) {
    return LinkStatus.values.firstWhere(
      (e) => e.value == value,
      orElse: () => LinkStatus.none,
    );
  }
}

enum IMFollowType {
  none(0),
  inMyFollowingList(1),
  inMyFollowersList(2),
  inBothFollowersList(3);

  final int value;

  const IMFollowType(this.value);

  static IMFollowType fromInt(int value) {
    return IMFollowType.values.firstWhere(
      (e) => e.value == value,
      orElse: () => IMFollowType.none,
    );
  }
}
