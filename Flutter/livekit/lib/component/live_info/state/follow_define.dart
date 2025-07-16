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
