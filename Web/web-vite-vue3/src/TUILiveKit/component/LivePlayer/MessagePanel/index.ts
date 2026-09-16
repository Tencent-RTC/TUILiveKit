// Barrel entry for the message panel — expose only what parents need.
// The emoji URL map (messageEmoji.ts) stays internal and is consumed via a
// relative import from MessagePanel.vue.
import MessagePanel from './MessagePanel.vue';

export default MessagePanel;
export { MessagePanel };
