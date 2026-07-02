# Mentor: An AI-Powered Personal Tutor

**Design Document — Ideal State**
**Status:** Draft v1 · July 2026
**Purpose:** This document describes the ideal state of an AI tutoring product. It is intentionally written *without reference to any existing implementation*, so it can serve as an independent benchmark for evaluating an existing project. Appendix A converts the design into a scorable evaluation rubric.

---

## 1. Vision

Anyone should be able to say *"I want to learn X"* and receive what a world-class private tutor provides: a personalized curriculum built on what they already know, lessons adapted to how they learn, honest assessment of their progress, and — critically — a system that ensures they *retain* what they learned instead of forgetting it weeks later.

The product closes the two failure modes of self-directed learning:

1. **The planning problem.** Learners don't know what they don't know. They can't sequence a subject, calibrate difficulty, or tell which resources matter.
2. **The forgetting problem.** Courses treat completion as the goal. Real learning is measured months later. Almost no product takes responsibility for retention.

**One-sentence product definition:** a tutor that plans your learning, teaches you adaptively, proves you learned it, and keeps proving it over time.

### 1.1 Who it's for

- **Now (v1):** a single motivated adult learner (the builder). No accounts, cohorts, or monetization concerns — the design effort goes entirely into the learning loop.
- **Later (v2+):** self-directed learners generally, with **professional upskilling** as the most promising wedge (e.g., "causal inference for data analysts"): learners with a job context, concrete goals, and willingness to do applied work.

The v1/v2 split is a scoping decision, not an architectural one: every core object below (learner profile, concept graph, scheduler) is designed to be per-user from day one.

### 1.2 Design principles

1. **Retention is the product.** Completion is an intermediate metric. The system is accountable for what the learner knows at T+90 days.
2. **Assessment drives everything.** The syllabus, the next lesson, the review queue — all are functions of an evidence-based model of what the learner knows.
3. **The learner sees the map.** Plans are visible and editable. The system is a guide, not a black box.
4. **Teach like a tutor, not a textbook.** Dialogue, probing, and adaptation to the person — their profession, examples that land for them, their misconceptions.
5. **Honesty over encouragement.** Mastery estimates must be calibrated. A tutor that inflates progress destroys the product's reason to exist.
6. **The AI is a guide, not the sole source of truth.** For contested or citation-heavy material, curate and discuss real sources rather than generating everything.

---

## 2. Core Concepts and Vocabulary

These are the nouns of the system. A candidate implementation should have a recognizable analog for each.

| Concept | Definition |
|---|---|
| **Learner Profile** | Durable, cross-course model of the person: background, profession, goals, preferences, explanation styles that work, history. |
| **Course** | A learning goal instantiated: "History of Western Philosophy," "Causal inference for data analysts." |
| **Concept** | The atomic unit of knowledge within a course (e.g., *confounding*, *Hume's problem of induction*). Courses decompose into concept graphs with prerequisite edges. |
| **Knowledge State** | Per-learner, per-concept mastery estimate with uncertainty and a decay model. The single source of truth for "what do they know right now." |
| **Syllabus** | A *living* ordered plan of units/lessons over the concept graph. A forecast, not a contract — re-planned as evidence arrives. |
| **Lesson** | One teaching interaction targeting specific concepts, in one of several modalities (§4.4). |
| **Session** | One sitting. May contain lesson work, review work, or both. |
| **Assessment** | Any evidence-producing activity: in-lesson checks, unit tests, graded projects, retention reviews. |
| **Review Queue** | The spaced-repetition schedule of concepts due for retrieval practice. |
| **Retention Check** | A periodic *holistic* mixed assessment of a completed course (distinct from atomic SRS reviews). |

---

## 3. Learning-Science Foundations

The design commitments below are the "why" behind the product mechanics. An ideal implementation reflects them; a naive one (generate text → show quiz → mark complete) violates most of them.

1. **Mastery learning (Bloom).** Don't advance past a concept until it's demonstrably learned. Time is the variable; mastery is the constant. Motivated by Bloom's two-sigma finding that individual tutoring + mastery learning dramatically outperforms classroom instruction — AI tutoring is a direct attempt to make that scalable.
2. **Retrieval practice (testing effect).** Recalling information strengthens memory far more than re-reading. Assessment *is* instruction, not just measurement — so the system tests constantly and treats quizzes as teaching moments.
3. **Spaced repetition & the forgetting curve.** Memory decays predictably; reviews timed near the forgetting point maximize retention per minute spent. Modern schedulers (FSRS-class) model per-item stability and difficulty.
4. **Desirable difficulties (Bjork).** Learning should feel slightly hard. Interleaving topics, varying contexts, and delayed testing feel worse but work better. The system must resist optimizing for "felt easy."
5. **Zone of proximal development / adaptivity.** Target material just beyond current ability. This requires a real diagnostic model — you cannot adapt to a learner you haven't measured.
6. **Generation and elaboration.** Explaining in your own words, applying to novel cases, and connecting to prior knowledge beat recognition. Hence Socratic dialogue and applied projects, and a grading system that scores free-form work — not just multiple choice.
7. **Transfer requires varied application.** Mastery displayed only in the training context is fragile. Retention checks deliberately vary surface features and demand application, not recitation.
8. **Metacognitive calibration.** Learners systematically overestimate what they know ("illusion of mastery" from fluent reading). The product shows calibrated mastery, including decay, and distinguishes recognition < recall < application < transfer.

---

## 4. Product Design

### 4.1 The core loop

```
  ENROLL ──► DIAGNOSE ──► PLAN ──► TEACH ──► ASSESS ──► UPDATE MODEL
                             ▲                               │
                             └──────── RE-PLAN ◄─────────────┘
                                                             │
                             (course complete) ──► RETAIN ◄──┘
                                     (SRS reviews + periodic retention checks,
                                      indefinitely, with remediation loops)
```

Everything in the product is one of these seven verbs. The loop never terminates: "course complete" transitions a course from *acquisition mode* to *maintenance mode*; it never leaves the system.

### 4.2 Enrollment & goal intake

The learner states a goal in natural language. The system conducts a short **goal interview** to establish:

- **Intent:** curiosity, job skill, credential/interview prep, teaching others — this changes depth, assessment style, and project design.
- **Scope negotiation:** the system proposes a scope ("Pre-Socratics through 20th century, survey depth, ~30 hours") and the learner adjusts.
- **Constraints:** time budget per week, target date if any, session length preference.
- **Prior exposure (self-report):** a first rough draft of the knowledge state, to be corrected by the diagnostic.

Output: a **Course Charter** — goal, scope, constraints, success definition ("you will be able to explain, compare, and apply…") that the learner approves.

### 4.3 Diagnostic onboarding

Before generating the syllabus, an **adaptive placement assessment** maps the learner against the course's concept graph:

- Conversational format (feels like a tutor chatting, not an exam), mixed with a few structured items for anchoring.
- Adaptive: branches toward the frontier of the learner's knowledge; skips regions they clearly know or clearly don't.
- Short — 10–20 minutes; the goal is a prior, not a final grade. The knowledge tracer will keep correcting it forever.
- Explicitly hunts for **misconceptions**, not just gaps (e.g., a data analyst who thinks "controlling for everything" is always safe).

Output: an initialized knowledge state over the concept graph, with uncertainty. Concepts the learner already has mastered enter the review queue directly rather than being re-taught.

### 4.4 The living syllabus

From the charter + diagnostic, the planner generates a full syllabus: units → lessons → target concepts, with estimated time, sequenced along prerequisite edges, skipping mastered material.

- **Visible and editable.** The learner sees the whole arc, can reorder, cut, or add topics ("skip the Scholastics," "go deeper on instrumental variables"). Edits are respected; the planner re-validates prerequisites and flags consequences.
- **Re-planned continuously.** After every session, the planner reconciles the syllabus with the updated knowledge state: insert remediation, compress mastered units, resequence. A visible changelog ("added a remedial lesson on colliders because the last project showed confusion") maintains trust.
- **A forecast, not a contract.** Progress is measured against concepts mastered, not lessons consumed.

### 4.5 Lesson modalities

Four modalities, chosen per-lesson by the planner based on concept type, learner preference, and evidence quality needed. A course mixes all four.

1. **Socratic dialogue.** Conversational tutoring: the tutor probes, poses cases, asks the learner to predict/explain/defend, and adapts in real time. Best for conceptual material and misconception repair. Every dialogue is also assessment — the tutor emits evidence about the learner's understanding as it goes.
2. **Generated reading + exercises.** A personalized expository lesson (tailored to the profile: examples from the learner's profession, building on what they know) followed by retrieval exercises spanning recognition → recall → application. Best for foundational/declarative material and learner-paced study.
3. **Applied projects.** Authentic tasks graded against rubrics: run a causal analysis on a provided dataset and defend the identification strategy; write 800 words comparing Hume and Kant on causation. Best for integration, transfer, and honest summative evidence. Rubric-based AI grading with concrete, actionable feedback and a revise-and-resubmit loop.
4. **Curated external content.** For citation-heavy, contested, or aesthetic material, the system curates real sources (papers, book chapters, lectures, videos), tells the learner *why this source and what to look for*, then debriefs and assesses via dialogue afterward. The AI is the guide and examiner, not the sole author.

**Selection heuristics:** declarative foundations → readings; conceptual subtlety or detected misconception → Socratic; integration milestones (unit ends) → projects; contested/primary-source material → curated content. The learner can always override ("just talk me through it").

### 4.6 Assessment architecture

Four layers, all writing evidence into one shared knowledge state:

| Layer | When | Form | Role |
|---|---|---|---|
| **Formative** | Continuously, inside every lesson | Dialogue probes, in-lesson exercises | Steer the lesson in real time; low-stakes, invisible-as-testing |
| **Summative (unit)** | End of each unit | Mixed assessment + often a project | Gate progression (mastery learning); trigger remediation |
| **Summative (course)** | End of course | Capstone project + comprehensive exam | Certify the learning goal from the charter was met |
| **Retention** | Forever after | SRS reviews + periodic retention checks | Maintain and verify durable knowledge (§4.7) |

Design requirements:

- **Beyond multiple choice.** Free-response, explanation, and applied work are first-class and AI-graded against rubrics. Recognition-only assessment inflates mastery.
- **Bloom-level tagging.** Every item targets a level (recall / understand / apply / analyze); mastery at "recall" does not imply mastery at "apply," and the knowledge state tracks the distinction.
- **Feedback is instruction.** Every wrong answer gets a targeted explanation and, where a misconception is detected, a named misconception recorded in the learner model.
- **Grading reliability.** Rubric-anchored grading, consistency checks, and learner ability to contest a grade (which is itself signal).
- **Anti-gaming honesty.** No mastery credit for re-reading or hint-heavy answers; hint usage discounts evidence strength.

### 4.7 The retention system

The differentiating subsystem. Two layers over one knowledge state:

**Layer 1 — Atomic spaced repetition.**
- Every concept (in-progress or completed courses alike) carries a memory model: stability, difficulty, predicted recall probability (FSRS-class scheduling).
- A daily **review queue** mixes due concepts *across all courses* — interleaving is a feature, not a bug.
- Reviews are retrieval-based and varied: not the same flashcard each time, but regenerated prompts — a recall question today, a mini-application next month, "explain it to a colleague" after that. Variation is what turns SRS from memorization into durable understanding.
- Review sessions are short (5–15 min) and the natural daily-habit surface of the product.

**Layer 2 — Periodic holistic retention checks.**
- On a decaying schedule after course completion (e.g., ~1, 3, 6, 12 months), a richer mixed assessment of the whole course: integrative questions, novel applications, a short synthesis task.
- Tests **transfer and integration**, which atomic reviews can't: surface features deliberately differ from the original lessons.
- Results flow back into the knowledge state; slipped areas trigger **remedial micro-lessons** injected into upcoming sessions — the RETAIN → TEACH loop.

**Honest decay display.** Mastery shown to the learner reflects predicted *current* recall, not peak historical performance. A course finished eight months ago with no reviews visibly fades — that honesty is what makes the retention promise credible.

### 4.8 Progress, mastery & the learner's view

- **Primary display: the concept map** — mastery (current, decay-adjusted) per concept, colored by strength, with uncertainty where evidence is thin. Progress = territory mastered, not percent of videos watched.
- **Course level:** charter goal status, units remaining, projected completion at current pace, syllabus changelog.
- **Retention level:** review-queue health, retention-check results over time, "knowledge at risk" (concepts predicted to slip soon).
- **Calibration view:** where learner self-assessment diverged from measured performance — training metacognition explicitly.

### 4.9 Motivation & habit (deliberately light in v1)

For a motivated personal user: streak-free, guilt-free design. A daily review nudge, session-end summaries of what strengthened, visible movement on the concept map, and honest "time to goal" projections. No badges, leagues, or engagement mechanics; if the product later serves broader consumers, this section gets rebuilt intentionally rather than bolted on.

### 4.10 Professional upskilling extensions (v2 sketch)

- **Skill taxonomies:** courses mapped to job-market skill frames so "causal inference for data analysts" composes from a shared graph rather than being invented per user.
- **Portfolio artifacts:** capstone projects rendered as shareable, verifiable work samples.
- **Role-aware planning:** "who I am" includes current role, target role, and the gap between them.
- **Team/employer surfaces:** manager-visible skill matrices — explicitly out of scope until the personal product proves the loop.

---

## 5. System Architecture (stack-agnostic)

### 5.1 Component overview

```
┌────────────────────────  Client (web-first; mobile = review surface)  ───────────────────────┐
│   Chat/lesson surface · Concept map · Syllabus editor · Review queue · Project workspace     │
└──────────────────────────────────────────┬───────────────────────────────────────────────────┘
                                           │  API
┌──────────────────────────────────────────▼───────────────────────────────────────────────────┐
│                                    Orchestrator                                              │
│         Session state machine · routes between services · assembles LLM context              │
└───┬──────────────┬──────────────┬──────────────┬──────────────┬──────────────────────────────┘
    │              │              │              │              │
┌───▼────┐   ┌─────▼─────┐  ┌─────▼─────┐  ┌─────▼─────┐  ┌─────▼─────┐
│Planner │   │  Tutor    │  │ Assessor  │  │ Curator   │  │ Scheduler │
│service │   │  engine   │  │ /Grader   │  │ (search/  │  │ (SRS +    │
│(syllabi,│  │(dialogue, │  │(item gen, │  │  RAG over │  │ retention │
│ concept │  │ lessons)  │  │ rubric    │  │  external │  │ checks)   │
│ graphs) │  │           │  │ grading)  │  │  content) │  │           │
└───┬────┘   └─────┬─────┘  └─────┬─────┘  └─────┬─────┘  └─────┬─────┘
    │              │              │              │              │
┌───▼──────────────▼──────────────▼──────────────▼──────────────▼─────┐
│                        Knowledge Tracer                             │
│   single write-path for evidence → per-concept mastery + decay      │
├─────────────────────────────────────────────────────────────────────┤
│   Stores: Learner Profile · Courses/Syllabi · Concept Graphs ·      │
│   Knowledge State · Assessment Evidence · Content/Lesson artifacts  │
├─────────────────────────────────────────────────────────────────────┤
│   Telemetry & Evals: traces, grading QA, tutor-quality eval suite   │
└─────────────────────────────────────────────────────────────────────┘
```

The essential architectural commitment: **separate the pedagogical roles.** Planner, tutor, grader, and curator are distinct prompted components with distinct contracts — not one mega-prompt. This enables independent evaluation and prevents role bleed (e.g., a tutor grading its own teaching generously).

### 5.2 Data model (entities)

- **LearnerProfile** — identity, background, profession, goals, preferences (modality, session length, explanation styles), misconception history, cross-course summary. Updated by a profile-maintenance step after sessions (episodic events → durable traits).
- **Course** — charter (goal, scope, constraints, success definition), status (diagnosing / active / maintenance), links to concept graph + syllabus.
- **ConceptGraph** — concepts with descriptions, Bloom-level targets, prerequisite edges. Generated per course; shared/canonical graphs are a v2 optimization.
- **Syllabus** — ordered units → planned lessons → target concepts; revision history with reasons.
- **KnowledgeState** — per (learner, concept): mastery estimate *per Bloom level*, uncertainty, memory parameters (stability/difficulty), last-evidence timestamp. Materialized view over the evidence log.
- **EvidenceEvent (append-only log)** — the system's ground truth: every answer, dialogue-extracted judgment, project rubric score, review outcome, with source, strength, and hint-discount. Mastery is always recomputable from the log.
- **Lesson / SessionTranscript** — generated artifacts and full interaction records.
- **AssessmentItem / RubricGrade** — items with concept + Bloom tags; grades with rubric criterion scores and feedback text.
- **ReviewSchedule** — per-concept due dates; per-course retention-check calendar.

### 5.3 LLM orchestration patterns

- **Structured outputs everywhere.** Planner emits syllabi as validated data, not prose; grader emits rubric scores as data; tutor dialogue turns carry a structured side-channel (`evidence[]`, `misconception?`, `concept_covered`) so teaching produces machine-readable assessment.
- **Context assembly, not context accumulation.** Each call gets a purpose-built context: relevant profile slice, target concepts + current mastery, last-session summary. Long-term memory lives in the stores, not in an ever-growing chat history; sessions are summarized on close.
- **Grading reliability.** Rubric-anchored prompts; grade-then-justify with self-consistency checks on high-stakes grades (unit/course summatives); periodic golden-set QA; learner contests logged as QA signal.
- **Knowledge tracing is code, not vibes.** The LLM produces *evidence*; a deterministic tracer (Bayesian/BKT-style update + FSRS-class decay) owns the mastery numbers. Never ask an LLM "so what's their mastery now?"
- **Factuality controls.** For citation-heavy domains, generation is grounded in retrieved sources (curator provides them); the tutor distinguishes consensus from contested claims; hallucination risk is part of the modality-selection heuristic (§4.5).
- **Model tiering.** Cheap/fast models for review-item generation and routine checks; strongest models for syllabus planning, Socratic teaching, and project grading.

### 5.4 Scheduling infrastructure

Retention requires the system to act *when the learner is absent*: a scheduler computes daily review queues, fires retention checks on the course calendar, and issues (configurable, restrained) nudges. This is a real background-jobs requirement — a purely request-driven app cannot deliver the retention promise.

### 5.5 Quality, evals & telemetry

The product's own claims must be testable:

- **Tutor-quality eval suite:** scripted learner personas (including ones with planted misconceptions) run against the tutor; judged on error detection, adaptation, and honesty. Run on every prompt/model change.
- **Grading QA:** golden set of pre-graded artifacts; drift monitoring.
- **Learning outcomes as the north-star metric:** retention-check scores at T+30/90/180 — not sessions, minutes, or completion.
- **Full traces** of every LLM interaction for debugging and offline eval.

### 5.6 Privacy & data

The learner model is intimate data (abilities, gaps, profession, goals). Requirements: learner can view and export everything the system believes about them; deletion is real; interaction data is not training fodder without explicit consent; profile inferences are inspectable and correctable ("you seem to prefer…" can be edited).

---

## 6. Scope & Phasing

**Phase 1 — the loop, single-user (the v1 bar):** goal intake + charter, diagnostic, living syllabus over a concept graph, Socratic + reading/exercise lessons, formative + unit assessment, knowledge tracer, SRS review queue, honest concept-map display. *If the retention layer is missing, it is not this product yet.*

**Phase 2 — depth:** applied projects with rubric grading, curated external content, periodic holistic retention checks with remediation, calibration view, profile maintenance loop, eval suite.

**Phase 3 — product:** multi-user, mobile review surface, shared/canonical concept graphs, professional-upskilling features (§4.10), motivation layer redesigned for a broad audience.

---

## 7. Risks & Open Questions

1. **Grading trust.** If learners catch the grader being wrong or inconsistent, the mastery model loses authority. Mitigations in §5.3; residual risk is real, especially for essays in contested domains.
2. **Concept-graph quality.** Everything keys off decomposition. A bad graph (too coarse, wrong prerequisites) silently corrupts planning and tracing. Needs its own eval attention.
3. **Illusion-of-adaptivity.** It's easy to *look* personalized while teaching everyone the same thing. The diagnostic and tracer must demonstrably change what gets taught.
4. **Review fatigue.** SRS queues grow with every completed course. Needs concept retirement policies, priority tiers ("maintain forever" vs "let fade"), and learner control.
5. **Hallucinated instruction.** Confidently wrong teaching is worse than no teaching. Grounding + curation for high-risk domains; factuality evals.
6. **Cost.** Strong-model Socratic dialogue is token-heavy. Tiering (§5.3) and caching help; the personal-use phase is the right time to measure real economics.
7. **Open:** how much learner control over the memory model? Where's the line between adaptive and paternalistic re-planning? Can retention checks stay engaging enough that they're actually taken?

---

## Appendix A — Evaluation Rubric

Score an existing project against the ideal state. Each dimension: **0** absent · **1** token/superficial · **2** present but partial · **3** substantially realized. Weights reflect this document's priorities.

| # | Dimension | What "3" looks like | Weight |
|---|---|---|---|
| A1 | Goal intake & charter | Interview establishes intent/scope/constraints; explicit approved success definition | 2 |
| A2 | Diagnostic onboarding | Adaptive placement initializes per-concept knowledge state; hunts misconceptions; mastered material skipped | 3 |
| A3 | Concept decomposition | Courses decompose into a concept graph with prerequisites; all planning/tracking keys off it | 3 |
| A4 | Living syllabus | Visible, editable, continuously re-planned from evidence, with changelog | 3 |
| A5 | Modality richness | ≥3 of: Socratic dialogue, generated reading+exercises, rubric-graded projects, curated external content — with principled selection | 2 |
| A6 | Personalization depth | Persistent cross-course profile demonstrably changes explanations, examples, and plans | 2 |
| A7 | Formative assessment | Continuous in-lesson evidence; dialogue itself produces structured evidence | 3 |
| A8 | Summative assessment | Unit gates (mastery learning) + course capstone against the charter; free-response and applied, Bloom-tagged | 3 |
| A9 | Knowledge tracing | Deterministic tracer over an append-only evidence log; per-concept, per-level mastery with uncertainty; not LLM-guessed | 3 |
| A10 | Spaced repetition | FSRS-class per-concept scheduling; cross-course interleaved queue; *varied regenerated* prompts, not static cards | 3 |
| A11 | Holistic retention checks | Periodic post-completion mixed assessments testing transfer; results drive remediation loops | 3 |
| A12 | Honest mastery display | Decay-adjusted current mastery; concept-map progress; calibration feedback | 2 |
| A13 | Role separation in orchestration | Planner/tutor/grader/curator as distinct contracted components with structured outputs | 2 |
| A14 | Context & memory architecture | Purpose-built context assembly; session summaries; durable stores as memory (not endless chat history) | 2 |
| A15 | Grading reliability | Rubrics, consistency checks, golden-set QA, contestability | 2 |
| A16 | Scheduling infrastructure | Background jobs deliver reviews/checks/nudges without user initiation | 2 |
| A17 | Evals & outcome telemetry | Tutor-quality eval suite; T+30/90 retention as north-star metric; full traces | 2 |
| A18 | Factuality controls | Grounded generation / curation for high-risk domains; consensus vs contested distinguished | 1 |
| A19 | Privacy & learner data rights | Inspectable/exportable/correctable learner model | 1 |
| A20 | Phasing discipline | Loop-first scope; retention not deferred; v2 features not crowding out v1 core | 1 |

**Scoring:** weighted sum / (3 × Σweights) → percentage. Suggested reading: **<40%** a lesson generator, not yet a tutor; **40–70%** a tutor without a memory — the acquisition loop exists but the retention promise doesn't; **>70%** the core product thesis is realized; remaining gaps are depth, not kind.

The highest-leverage question when evaluating any implementation: **A9 + A10 + A11 — does the system take responsibility for what the learner knows six months later?** That triad is where this product differs from "ChatGPT, teach me philosophy."

## Appendix B — Example Stack Instantiation (non-normative)

One concrete way to build §5, purely illustrative; the reference design is the contract, not these choices. Web client (React/Next.js) with a chat-centric lesson surface and an interactive concept-map view · API + orchestrator in a typed server runtime (TypeScript/Node or Python/FastAPI) · Postgres for profiles/courses/graphs/evidence log (append-only table + materialized knowledge-state view) · a job runner (e.g., Temporal, or cron + queue) for the scheduler · LLM access through a thin provider-agnostic layer with structured-output validation (JSON Schema), strongest model tier for planner/tutor/grader, small model tier for review-item generation · FSRS library for memory scheduling · trace logging of all LLM calls to an observability store with an offline eval harness (persona scripts + golden grading sets) run in CI.
