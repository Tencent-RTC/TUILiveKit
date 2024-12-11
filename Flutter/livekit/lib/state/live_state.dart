import 'package:tencent_live_uikit/state/operation/operation_state.dart';
import 'package:tencent_live_uikit/state/view/view_state.dart';

class LiveState {
  final OperationState operationState = OperationState();
  final ViewState viewState = ViewState();

  void reset() {
    viewState.reset();
    operationState.reset();
  }
}
