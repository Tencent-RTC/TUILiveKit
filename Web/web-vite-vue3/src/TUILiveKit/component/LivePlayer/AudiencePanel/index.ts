// Barrel entry for the audience panel — expose only what parents need.
// Internal files (AudienceCard, penguinSeats) stay accessible via relative
// imports inside this folder but are not part of the public surface, keeping
// the domain's coupling seam sharp.
import AudiencePanel from './AudiencePanel.vue';

export default AudiencePanel;
export { AudiencePanel };
