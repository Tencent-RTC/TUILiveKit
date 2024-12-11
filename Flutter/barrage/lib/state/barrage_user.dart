class BarrageUser {
  String userId = "";
  String userName = "";
  String avatarUrl = "";
  String level = "";

  @override
  String toString() {
    return "BarrageUser{userId:$userId,userName:$userName,avatarUrl:$avatarUrl,level:$level}";
  }

  @override
  bool operator ==(Object other) {
    if (identical(this, other)) return true;
    return other is BarrageUser && other.userId == userId;
  }

  @override
  int get hashCode => userId.hashCode;
}
