import LivePlayerH5 from './LivePlayerH5.vue';
import LivePlayerPC from './LivePlayerPC.vue';

import { isMobile } from '../../utils/environment';

const LivePlayer = isMobile ? LivePlayerH5 : LivePlayerPC;

export default LivePlayer;
