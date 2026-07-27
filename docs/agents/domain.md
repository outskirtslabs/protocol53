# Domain Docs

How the engineering skills should consume this repo’s domain documentation when exploring the codebase.

## Layout

This repository uses a single domain context shared by the API, testkit, and provider subprojects.

- `CONTEXT.md` at the repository root contains the domain glossary and model.
- `docs/adr/` contains repository-wide architectural decisions.

These files are created lazily when domain terms or decisions are resolved.

## Before exploring, read these

- Read `CONTEXT.md` if it exists.
- Read ADRs under `docs/adr/` that touch the area about to be changed.
- If either path does not exist, proceed silently. Do not flag its absence or suggest creating it upfront.

## File structure

```text
/
├── CONTEXT.md
├── docs/
│   └── adr/
├── api/
├── providers/
└── testkit/
```

## Use the glossary’s vocabulary

When output names a domain concept—in an issue title, refactor proposal, hypothesis, or test name—use the term defined in `CONTEXT.md`. Do not drift to synonyms the glossary explicitly avoids.

If a needed concept is absent, reconsider whether the term belongs to the project. If it exposes a real gap, note it for Skill(domain-modeling).

## Flag ADR conflicts

If output contradicts an existing ADR, surface it explicitly rather than silently overriding it:

> _Contradicts ADR-0007 (event-sourced orders)—but worth reopening because…_
