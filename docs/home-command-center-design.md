# Home Command Center — Design Document

**Status:** Draft for review
**Author:** Claude (interviewed brentmwill@gmail.com)
**Date:** 2026-07-02
**Purpose:** This document describes the intended design of a self-hosted home
command center. It is deliberately written *independently of the existing
codebase* so it can later be compared against the current implementation to
surface gaps, produce ergonomics/UX recommendations, and generate tickets.

---

## 1. Overview

A self-hosted household hub with two faces:

1. **A kiosk** — an always-on touchscreen in a shared space showing the
   ambient state of the household: shared calendar, upcoming events, chores,
   a rotating photo slideshow, and the current meal plan.
2. **A phone-accessible app** — the same system reached from personal devices
   over a Tailscale network, used for the interactive work: planning meals,
   managing recipes, and checking off a grocery list in the store.

The **central loop** of the product is meal planning:

> maintain a recipe library → compose recipes into a weekly meal plan →
> generate a consolidated grocery list → shop against that list →
> rate what was cooked → feed ratings back into recommendations.

Everything else on the kiosk (calendar, chores, photos) is ambient context
around that loop.

### 1.1 Goals

- One glanceable surface for the household's day: events, chores, meals.
- A recipe library with structured ingredients, instructions, and nutrition.
- Frictionless meal-plan → grocery-list generation with sane ingredient
  consolidation.
- A built-in shopping list good enough to **replace AnyList**, including the
  general-purpose (non-grocery) list functionality currently paid for.
- Recipe recommendations driven by an internal rating system and, later, by
  ingredient availability.
- Self-hosted; household data stays on household hardware.

### 1.2 Non-goals (for now)

- Per-user accounts, permissions, or private data. The household is modeled
  as a **single shared unit** — one calendar view, one chore list, one set
  of ratings.
- Full pantry inventory with quantity tracking and automatic deduction
  (explicitly deferred; see §6).
- Public internet exposure. Remote access is via Tailscale only.
- Native mobile apps. Phone use is via responsive web (or PWA).

### 1.3 Future considerations (design for, don't build)

- **Monarch Money integration** — surface budget and spending summaries on
  the kiosk (see §8.4).
- Pantry quantities and depletion tracking.
- Photo/vision-assisted pantry hydration ("photograph the shelf").

---

## 2. Usage contexts

The same system serves three distinct interaction modes, and the design of
every feature should state which mode(s) it targets:

| Mode | Device | Interaction | Typical tasks |
|---|---|---|---|
| **Ambient** | Kiosk, viewed from across the room | None (glanceable) | Today's events, tonight's dinner, chore status, photos |
| **Kiosk-touch** | Kiosk, standing at it | Brief touches, < 30 s | Check off a chore, mark a meal cooked, flip to the week view |
| **Phone** | Personal phone over Tailscale | Full interaction | Plan the week, import a recipe, shop with the list, rate a meal |

**Design rule:** anything requiring more than ~3 taps or any typing belongs to
the phone mode. The kiosk must never present a keyboard as part of a routine
flow.

---

## 3. Architecture

Stack-agnostic by request. The design commits to *boundaries and contracts*,
not languages or frameworks.

```
┌────────────────────────────── Home server (self-hosted) ─────────────────────────────┐
│                                                                                      │
│  ┌────────────┐   ┌───────────────────────────────┐   ┌──────────────────────────┐   │
│  │  Kiosk UI  │   │        Core API service       │   │      Sync workers        │   │
│  │ (fullscreen│──▶│  REST/JSON (or GraphQL) over  │◀──│  - Google Calendar poll  │   │
│  │  browser)  │   │  HTTP + a push channel (SSE/  │   │  - iCloud CalDAV poll    │   │
│  └────────────┘   │  WebSocket) for live updates  │   │  - Recipe URL importer   │   │
│  ┌────────────┐   │                               │   │  - Photo folder watcher  │   │
│  │  Phone UI  │──▶│  Domain modules:              │   │  - Nutrition resolver    │   │
│  │ (responsive│   │   recipes · plans · lists ·   │   └──────────────────────────┘   │
│  │  web/PWA)  │   │   pantry · chores · calendar  │   ┌──────────────────────────┐   │
│  └────────────┘   │   · photos · ratings          │   │   Storage                │   │
│                   └───────────────────────────────┘   │  - Relational DB (single │   │
│                                                       │    file DB is sufficient)│   │
│         Access: LAN + Tailscale (tailnet-only;        │  - Photo folder (bind-   │   │
│         no ports exposed to the public internet)      │    mounted, read-only)   │   │
│                                                       └──────────────────────────┘   │
└──────────────────────────────────────────────────────────────────────────────────────┘
```

### 3.1 Components

- **Core API service** — owns the domain model and all writes. Every UI and
  worker goes through it. This is the contract surface to compare against the
  existing codebase.
- **Kiosk UI** — a fullscreen browser (kiosk mode) pointed at a `/kiosk`
  route. Subscribes to the push channel so the display updates without
  refresh. Must tolerate the server restarting (auto-reconnect) and render
  acceptably from cached state if the network blips.
- **Phone UI** — responsive web app on the same origin, reachable over the
  tailnet. Installable as a PWA so the shopping list works with a
  home-screen icon and (ideally) an offline cache for in-store use where
  Tailscale connectivity may be spotty.
- **Sync workers** — background jobs, isolated from the request path:
  calendar polling, recipe import parsing, photo-folder indexing, nutrition
  lookup. Failures in a worker must never take down the kiosk.
- **Storage** — a single relational database (an embedded/single-file DB is
  sufficient at household scale) plus the photo directory, which the system
  treats as **read-only source of truth** — it indexes and displays photos
  but never modifies the folder.

### 3.2 Deployment & operations

- Packaged as containers (or equivalent) on the home server; one command to
  bring the whole stack up.
- Nightly automated backup of the database to a second location (NAS folder,
  cloud object storage, or even a git-tracked dump). Restore procedure
  documented and tested.
- No authentication *inside* the tailnet initially (the network boundary is
  the auth boundary), but the API should be written so a simple auth layer
  can be added without restructuring — e.g., all routes behind one
  middleware seam.

---

## 4. Domain model

The entities below are the vocabulary of the whole system. Names matter more
than storage details; the comparison exercise against the existing code
should start here.

```
Recipe ─────────┬── RecipeIngredient ──▶ Ingredient (canonical)
  │             └── RecipeStep
  │
  ├── NutritionFacts (per serving)
  ├── Rating (0..n over time)
  └── Tag (cuisine, meal type, effort, etc.)

MealPlan (a week) ── MealSlot (date × meal) ──▶ Recipe (0..n per slot)

List (generic!) ── ListItem ──[optional]──▶ Ingredient
GroceryList = a List generated from a MealPlan (retains provenance)

PantryItem ──▶ Ingredient           (lightweight "on hand" flag; see §6)

Chore ── ChoreOccurrence (recurrence-generated, completable)
CalendarSource ── CalendarEvent (read-only mirror of external calendars)
PhotoIndexEntry (path, taken-at, display eligibility)
```

Key modeling decisions:

- **`Ingredient` is a canonical, deduplicated entity**, separate from
  `RecipeIngredient` (which adds quantity, unit, and preparation note, e.g.
  "2 cups onion, diced"). This is the linchpin: grocery-list consolidation,
  pantry matching, and ingredient-based recommendations all depend on two
  recipes agreeing that "onion" is the same thing. Import flows must map
  parsed ingredient text to canonical ingredients, with a human-confirmable
  fallback when parsing is ambiguous.
- **`List` is generic, not grocery-specific.** A grocery list is just a
  `List` whose items carry ingredient links and meal-plan provenance. This
  is what makes the system an AnyList *replacement* rather than a grocery
  exporter: the same lists feature handles "Hardware store," "Packing —
  camping trip," "Gift ideas." See §5.5.
- **Ratings are events, not a single mutable score.** Each time a recipe is
  cooked, the household can record a rating (and optionally a note: "kids
  loved it," "too salty"). The recipe's displayed score is derived
  (e.g., recency-weighted average), and the rating history doubles as a
  "when did we last have this?" signal for recommendations.
- **External calendar events are a read-only mirror.** The system never
  writes to Google/iCloud; it caches events locally so the kiosk works
  during upstream outages.
- **`MealSlot` holds zero-or-more recipes** (a dinner can be a main + a
  side) and may alternatively hold a free-text entry ("leftovers,"
  "eating out") that generates no groceries.

---

## 5. Feature specifications

### 5.1 Kiosk dashboard (Ambient + Kiosk-touch)

The kiosk's default screen is a dashboard composed of cards:

- **Today & upcoming** — merged agenda from all calendar sources for today
  plus a configurable look-ahead (default 7 days). Color/labeled by source
  calendar.
- **Tonight's meal** (and tomorrow's, smaller) — pulled from the active
  `MealPlan`. Tapping opens the recipe in a kiosk-friendly **cook view**:
  large type, step-by-step, screen stays awake. This is the one rich
  interactive surface the kiosk earns, because the kitchen is where the
  kiosk lives.
- **Chores** — due/overdue occurrences with a single-tap complete.
- **Photo slideshow** — takes over the full screen after N minutes of
  inactivity (screensaver behavior); any touch returns to the dashboard.
  Sources from the indexed photo folder; shuffle with recency bias;
  honors an exclusion mechanism (e.g., skip subfolders prefixed `_`).
- **Grocery-list badge** — count of unchecked items on the active list, as
  a passive "shopping is pending" signal.

Ambient requirements: legible from ~3 m for the headline items; automatic
day/night brightness or theme; clock and date always visible; degrades
gracefully (a failed calendar sync shows last-known data with a subtle
staleness indicator, never an error wall).

### 5.2 Calendar aggregation

- Sources: **Google Calendar** (API, OAuth) and **iCloud** (CalDAV with an
  app-specific password). Multiple calendars per source; each mapped to a
  display color/label.
- Read-only. Poll on an interval (e.g., 5–15 min) via a sync worker; store
  a local mirror; the kiosk renders only from the mirror.
- Recurring-event expansion happens at sync time for the display window.
- Setup UX: adding a source is a phone-mode flow; the kiosk only displays.

### 5.3 Recipe library

**Fields:** title, image, servings, prep/cook time, tags, source URL/attribution,
ingredients (quantity + unit + canonical ingredient + prep note), ordered
steps, per-serving nutrition, rating history, "last cooked" (derived).

**Entry paths (all phone-mode):**

1. **Import from URL** — fetch the page, parse schema.org/Recipe JSON-LD or
   microdata (the vast majority of recipe sites carry it), fall back to
   heuristic HTML extraction. Always land in a **review screen** — parsed
   ingredients mapped to canonical ingredients with confidence indicators —
   before saving. Never silently save a bad parse.
2. **Photo/scan import** — photograph a cookbook page or recipe card; OCR
   to text; run the same ingredient/step parser; same review screen. This
   path is explicitly best-effort and its review step will do more work.
3. **Manual entry** — a structured editor, also used to edit anything the
   importers produced.

**Nutrition:** prefer nutrition data present in the imported markup; where
absent, resolve per-ingredient against a nutrition database (e.g., USDA
FoodData Central) and sum per serving, clearly marked as *estimated*.
Nutrition display is per-serving on the recipe and summable across a day's
meal slots later.

### 5.4 Meal planning (the core loop)

Phone-mode planning surface; kiosk displays the result.

- Week grid: days × meal types (configurable; default dinner-only rows can
  expand to breakfast/lunch).
- Fill a slot by: searching the library, picking from **recommendations**
  (§5.6), repeating a recent plan, or free-text (no grocery impact).
- A slot serving-size override scales ingredient quantities.
- **Plan → list:** one action generates/updates the grocery list for the
  plan: union all ingredients across planned recipes, scale by servings,
  **consolidate by canonical ingredient with unit normalization** (2 recipes
  × "1 cup onion" + "½ onion" → one line, best-effort combined, with
  provenance showing which recipes contributed). Items the household flags
  as always-stocked staples (salt, oil) are auto-excluded but visible in a
  collapsed "assumed on hand" section — trust but verify.
- Re-generating after a plan edit **preserves manual list state**: checked
  items stay checked, hand-added items stay, removed-recipe items are
  flagged rather than silently deleted.
- Marking a slot **cooked** (kiosk-touch or phone) prompts an optional
  rating and stamps the recipe's history.

### 5.5 Lists (grocery and general-purpose)

This is the AnyList-replacement surface, so it is deliberately more than a
grocery feature:

- Any number of named lists; grocery lists are just lists with provenance.
- Items: free text, optional link to a canonical ingredient, optional
  quantity/note, checkable, reorderable.
- **Grocery ergonomics:** aisle/category grouping (categories on canonical
  ingredients, learnable from corrections), check-off with undo, "recently
  checked" tail for accidental taps, works one-handed.
- **Shared and live:** two phones and the kiosk see check-offs in near-real-
  time via the push channel.
- **Offline-tolerant on the phone:** the PWA caches the active list and
  queues check-offs/adds made while disconnected, syncing on reconnect
  (last-write-wins per item is acceptable at household scale).
- **Export escape hatches:** any list can be copied as plain text/markdown
  and (nice-to-have) printed — so the built-in list never locks data in.
- **Extensibility contract:** lists are fully manipulable via the API
  (create list, add/check/remove items) with a stable, documented surface —
  this is what "interact with it some other way" requires. Concretely this
  enables: Siri/Shortcuts or voice-assistant bridges ("add milk to the
  grocery list"), automations, and future integrations without UI changes.
  *Recommendation:* treat "add an item by voice from a phone" as an
  acceptance criterion for AnyList parity, since capture-by-voice is a big
  part of what such apps are paid for.

### 5.6 Recommendations

Two engines, both surfaced as pickable suggestions during planning (never
auto-committed):

1. **Rating/rotation-based (build first):** score = recency-weighted rating,
   boosted by "haven't had it in a while," filtered by tags (e.g., "weeknight,"
   "≤30 min"). Deterministic and explainable: every suggestion shows *why*
   ("rated 4.5, last cooked 6 weeks ago").
2. **Availability-based (build on the pantry list, §6):** rank recipes by
   fraction of ingredients currently on hand; show the gap ("you have 6 of 8;
   missing: cream, thyme"). Also available as an ad-hoc query ("what can we
   make with chicken and rice?") independent of the pantry list.

### 5.7 Chores

- Recurring chore definitions (daily/weekly/monthly/custom) generate
  occurrences; single shared household, so no assignment/rotation — just
  done/not-done with a completion timestamp.
- Kiosk-touch check-off is the primary interaction; definitions are managed
  in phone mode.
- Overdue items visually escalate on the kiosk (gently — this is a family
  dashboard, not a CI wall).

### 5.8 Photos

- Watch a configured folder (local path or network mount); index new files;
  read EXIF for taken-date ordering; never write to the folder.
- Slideshow behavior per §5.1. Config: transition interval, shuffle vs.
  chronological, subfolder include/exclude.

---

## 6. Pantry: design for the future, build for the present

Per the interview: full inventory systems are hard to maintain, so the
initial build is a **lightweight "on hand" list** — a checklist of canonical
ingredients with no quantities — while the data model leaves room to grow.

- **Now:** `PantryItem(ingredient_id, on_hand: bool, updated_at)`. Toggled
  manually; used only by the availability recommender and the "assumed on
  hand" grocery exclusion.
- **Designed-for later (schema allows, features deferred):**
  - Optional `quantity`/`unit` columns, unused initially.
  - **Hydration hooks:** completing a grocery shop can offer "mark purchased
    items as on hand" (one tap, no quantities). Marking a meal cooked can
    offer to clear its perishable ingredients. Both are batch *suggestions*,
    never automatic bookkeeping.
  - **Vision-assisted hydration** (photograph the pantry/fridge shelf,
    detect items, confirm as a batch) — an explicitly experimental future
    workflow; the API just needs a batch-upsert endpoint for it.
- **Honesty rule:** availability recommendations must be phrased as
  best-effort ("likely on hand") since the list will drift; the UI should
  make correcting drift one tap from wherever the stale data is displayed.

---

## 7. Non-functional requirements

- **Resilience:** kiosk survives server restarts and upstream (Google/
  iCloud) outages by rendering cached state with staleness indicators.
- **Performance:** kiosk dashboard interactive < 2 s after boot; list
  check-off round-trip feels instant (optimistic UI).
- **Privacy:** all data on household hardware; no third-party analytics;
  outbound traffic limited to the configured integrations (calendar,
  recipe-URL fetches, nutrition lookups).
- **Backups:** nightly DB backup + documented restore; photos are already
  the household's own folder and are not duplicated by the system.
- **Maintainability at household scale:** one maintainer, occasional
  attention. Prefer boring technology, few moving parts, and loud health
  reporting (a `/health` status the kiosk can show discreetly) over
  sophistication.

---

## 8. Integrations

### 8.1 Tailscale
The tailnet is the access and (initial) auth boundary. Serve over the
tailnet; do not expose ports publicly. Document the kiosk's device as a
tailnet node or place it on the LAN with the server.

### 8.2 Calendars
Google Calendar API (OAuth, refresh tokens stored server-side) and iCloud
CalDAV (app-specific password). Both handled by sync workers; see §5.2.

### 8.3 List export / bridges
Built-in lists are primary (§5.5). Bridges (in likely order of value):
plain-text/markdown copy → iOS Shortcuts/Siri via the API → voice-assistant
shopping-list push if still wanted after the built-in list proves itself.
An AnyList one-time import (recipes and/or lists) is desirable for
migration if export tooling permits.

### 8.4 Monarch Money (future)
A read-only budget card on the kiosk: current month spending vs. budget by
a few chosen categories. Constraints to design around: credentials must
stay server-side; data is cached and clearly timestamped; the card is
ambient-only (no drill-down on the kiosk). Not scheduled; listed so the
dashboard's card system is built pluggable enough to accept it.

---

## 9. Phasing

Each phase is independently useful; the core loop lands in Phase 2.

- **Phase 1 — The glanceable kiosk:** kiosk shell + dashboard cards,
  calendar sync (Google + iCloud), photo slideshow, chores. *Outcome: the
  screen on the wall is already worth having.*
- **Phase 2 — The core loop:** recipe library with URL import + manual
  entry, meal plan week grid, plan → consolidated grocery list, built-in
  shared live list with phone check-off, cook view + ratings capture.
  *Outcome: AnyList grocery use case replaced.*
- **Phase 3 — Parity and intelligence:** general-purpose lists + API/
  Shortcuts bridge (full AnyList replacement), rating/rotation
  recommendations, nutrition resolution for import gaps, offline PWA
  hardening for in-store use.
- **Phase 4 — Availability & stretch:** lightweight pantry list +
  availability recommender + hydration hooks, photo/scan recipe import,
  aisle-category learning, Monarch card.

---

## 10. Open questions

1. **Kiosk hardware** — existing tablet vs. dedicated screen + SBC affects
   the auto-brightness/wake story but not the architecture.
2. **AnyList migration** — is exporting existing recipes/lists from AnyList
   feasible enough to justify a one-time importer, or is re-entry during
   Phase 2 acceptable?
3. **Voice capture priority** — how essential is "add milk by voice" to
   daily habit? It determines whether the Shortcuts/assistant bridge is
   Phase 3 or later.
4. **Nutrition rigor** — is estimated per-serving nutrition informational,
   or will it be used for actual dietary tracking (which raises the bar on
   ingredient-matching accuracy)?
5. **Meal types** — dinner-only to start, or breakfast/lunch slots from day
   one?

---

## 11. How this document will be used

This design was written blind to the existing implementation, on purpose.
Next steps:

1. **Comparison pass:** map each section (especially §4 domain model and §5
   feature specs) against the existing project; classify each element as
   *present / partial / absent / divergent*.
2. **Recommendations:** where the implementations diverge, decide which
   direction wins, with UX ergonomics (§2's interaction-mode rules, §5.5's
   list ergonomics) as the tiebreaker.
3. **Tickets:** turn the gap list into scoped tickets, grouped by the
   phases in §9.
