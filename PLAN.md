# Meowderall Implementation Plan

## Phase 1: Project Setup
1. **Initialize Elm project** - `elm init`, `elm.json`
2. **Create justfile** - Build, dev server, GCP deploy commands
3. **Create Dockerfile** - nginx serving static Elm build
4. **Touch-friendly CSS** - Large tap targets, mobile-first responsive design

## Phase 2: Core UI (iPad-optimized)
5. **Medication form view** - Display cat info, treatment details, date range
6. **Daily tracking table** - Large, tappable cells for AM/PM initials
7. **Tap-to-edit initials** - Tap a cell → opens keyboard/picker for initials
8. **Comments field** - Easy text input for daily notes

## Phase 3: Auth & Permissions
9. **PIN modal** - Large numpad UI for 4-digit entry (touch-friendly)
10. **Admin gating** - AM/PM initials require PIN, other fields open to all
11. **Session persistence** - Remember admin status in localStorage

## Phase 4: Data & Deploy
12. **LocalStorage persistence** - Save form state in browser
13. **(Optional) CSV import** - Load existing paper forms if needed
14. **GCP deploy** - `just gcp-deploy-all`

---

## Key iPad UX considerations
- Minimum 44x44px tap targets
- Large fonts for readability
- No hover states (touch-only)
- Responsive layout for landscape/portrait
