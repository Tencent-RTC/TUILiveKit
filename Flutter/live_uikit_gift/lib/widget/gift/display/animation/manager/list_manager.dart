class ListManager<T> {
  final int maxLength;
  final List<T> _storage = [];
  int _totalCount = 0;

  int get totalCount => _totalCount;
  int get count => _storage.length;

  ListManager({this.maxLength = 1000});

  void removeAll() {
    _storage.clear();
  }

  T? popFirst() {
    if (_storage.isEmpty) return null;
    return _storage.removeAt(0);
  }

  void insert(T obj, int index) {
    if (index >= count) {
      append(obj);
      return;
    }

    _totalCount++;
    if (_storage.length + 1 > maxLength) {
      _storage.removeAt(0);
    }
    _storage.insert(index, obj);
  }

  void append(T obj) {
    _totalCount++;
    if (_storage.length + 1 > maxLength) {
      _storage.removeAt(0);
    }
    _storage.add(obj);
  }

  void appendAll(List<T> list) {
    _totalCount += list.length;

    if (list.length > maxLength) {
      _storage.addAll(list.sublist(list.length - maxLength));
      return;
    }

    if (_storage.length + list.length > maxLength) {
      final removeCount = _storage.length + list.length - maxLength;
      _storage.removeRange(0, removeCount);
    }
    _storage.addAll(list);
  }

  T? removeAt(int index) {
    if (index < 0 || index >= _storage.length) return null;
    return _storage.removeAt(index);
  }

  T? reverse(int index) {
    if (index < 0 || index >= _storage.length) return null;
    final reverseIndex = _storage.length - 1 - index;
    return _storage[reverseIndex];
  }

  T? operator [](int index) {
    if (index < 0 || index >= _storage.length) return null;
    return _storage[index];
  }

  List<T> getList() => List<T>.unmodifiable(_storage);

  List<T> call() => getList();
}