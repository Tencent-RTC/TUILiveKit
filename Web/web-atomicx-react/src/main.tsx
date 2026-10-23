import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
// Registers the room-engine login listener before any login().
import 'tuikit-atomicx-react/live';
import { App } from './App';
import { addDemoI18n } from './i18n';
import { restoreLogin } from './utils/login';

addDemoI18n();

void restoreLogin().then(() => {
  createRoot(document.getElementById('root')!).render(
    <StrictMode>
      <App />
    </StrictMode>
  );
});
