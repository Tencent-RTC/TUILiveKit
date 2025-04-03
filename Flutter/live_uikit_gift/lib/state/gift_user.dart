class GiftUser {
  String? userId = "";
  String? userName = "";
  String? avatarUrl = "";
  String? level = "";

  GiftUser({
    this.userId,
    this.avatarUrl,
    this.userName,
    this.level,
  });

  factory GiftUser.fromJson(Map<String, dynamic> json) {
    return GiftUser(
      userId: json['userId'],
      avatarUrl: json['avatarUI'],
      userName: json['userName'],
      level: json['level'],
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'userId': userId,
      'avatarUrl': avatarUrl,
      'userName': userName,
      'level': level,
    };
  }

  @override
  String toString() {
    return 'GiftUser{userId: $userId, userName: $userName, avatarUrl: $avatarUrl, level: $level}';
  }

  void updateUserInfo(GiftUser owner) {
    userName = owner.userName;
    avatarUrl = owner.avatarUrl;
    level = owner.level;
  }
}