export function FieldHint({ content }: { content: string }) {
  return (
    <span className="field-hint">
      <button
        type="button"
        className="field-hint-trigger"
        aria-label={content}
        tabIndex={0}
      >
        <svg width="14" height="14" viewBox="0 0 24 24" fill="none" aria-hidden="true">
          <circle cx="12" cy="12" r="9.2" stroke="currentColor" strokeWidth="1.7" />
          <path
            d="M9.6 9.2a2.45 2.45 0 1 1 3.4 2.26c-.63.28-1 .78-1 1.42v.52"
            stroke="currentColor"
            strokeWidth="1.7"
            strokeLinecap="round"
          />
          <circle cx="12" cy="16.5" r="0.95" fill="currentColor" />
        </svg>
      </button>
      <span className="field-hint-bubble" role="tooltip">{content}</span>
    </span>
  );
}
