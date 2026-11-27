import { RouterProvider } from 'react-router-dom';
import { i18next, UIKitProvider } from '@tencentcloud/uikit-base-component-react';
import { toLiveLanguage } from '@tencentcloud/livekit-web-react';
import { router } from './router';

const initialLanguage = toLiveLanguage(i18next.language || navigator.language);

export function App() {
  return (
    <UIKitProvider theme="dark" language={initialLanguage}>
      <RouterProvider router={router} />
    </UIKitProvider>
  );
}
