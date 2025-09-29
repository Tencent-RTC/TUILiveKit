enum LiveStreamPrivacyStatus { public, privacy }

enum LiveStatus { none, previewing, pushing, playing, finished }

enum LiveTemplateMode {
  verticalDynamicGrid(id: 600),
  verticalDynamicFloat(id: 601),
  verticalStaticGrid(id: 800),
  verticalStaticFloat(id: 801);

  final int id;

  const LiveTemplateMode({required this.id});
}
