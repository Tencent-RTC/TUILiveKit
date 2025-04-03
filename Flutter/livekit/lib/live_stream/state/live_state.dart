import 'index.dart';

class LiveState {
  final OperationState operationState = OperationState();
  final ViewState viewState = ViewState();

  void reset() {
    viewState.reset();
    operationState.reset();
  }
}
