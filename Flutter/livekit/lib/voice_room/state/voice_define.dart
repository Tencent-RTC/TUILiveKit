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