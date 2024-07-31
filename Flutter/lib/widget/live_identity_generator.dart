class LiveIdentityGenerator {
  static final LiveIdentityGenerator instance = LiveIdentityGenerator._internal();

  factory LiveIdentityGenerator() {
    return instance;
  }

  LiveIdentityGenerator._internal();

  String generateId(String id, RoomType type) {
    return type.prefix + id;
  }

  RoomType? getIDType(String? id) {
    if (id == null) {
      return null;
    }
    for (RoomType type in RoomType.values) {
      if (id.startsWith(type.prefix)) {
        return type;
      }
    }
    return null;
  }
}

enum RoomType {
  live('live_'),
  voice('voice_');

  final String prefix;

  const RoomType(this.prefix);
}
