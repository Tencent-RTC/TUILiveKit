import { RouterProvider } from 'react-router-dom';
import { UIKitProvider } from '@tencentcloud/uikit-base-component-react';
import { router } from './router';

export function App() {
  return (
    <UIKitProvider theme="dark">
      <RouterProvider router={router} />
    </UIKitProvider>
  );
}
