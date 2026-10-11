# AGENTS.md

Single source of truth for agents working on **selection-screen**.

Ranges, filters and selection screens with persisted variants.

## What this repository is

A **source** repository in the abap2UI5 ecosystem: humans and agents edit here,
and CI gates every change. It is installed with abapGit and depends on the
[abap2UI5](https://github.com/abap2UI5/abap2UI5) core at runtime.

## Layout

| Path | Contents |
| --- | --- |
| `src/00/` | Samples (`z2ui5_cl_sel_sample_*`) |
| `src/01/` | The selection screen itself, its popups and the variant handling |

## Dependencies

- [abap2UI5](https://github.com/abap2UI5/abap2UI5) — the framework
- [layout-management](https://github.com/abap2UI5-addons/layout-management) — variant persistence

There is no version pin on any of them — abaplint resolves each from its `main`
branch. That is why the check workflows also run on a weekly schedule: a rename
upstream breaks this repository silently, and with no pull request open nothing
else would notice.

## Build and verify

```sh
npm ci
npm run check
```

`npm run check` runs exactly what CI runs. See [CONTRIBUTING.md](CONTRIBUTING.md)
for what each gate proves and how the abap2UI5-linter baseline works.

## Views

Views are built with **`z2ui5_cl_ui5_view_builder`**, the core's generic builder
in `src/02`: `ele` (add a child and descend), `tag` (add a child and stay), `a`
(set an attribute on the element the chain points at), `end` (ascend),
`stringify` (render from the root).

Three rules that are easy to get wrong:

1. **`a( )` targets the last child** once a node has children, so a control's
   attributes belong immediately after the `ele( )`/`tag( )` that opened it.
2. **An ABAP boolean goes into `b =`.** Through `v =` an `abap_false` renders as
   an empty string, and UI5 reads an empty string as true.
3. **The root declares its own namespaces.** The frozen builder collected them
   and wrote the `xmlns` attributes itself; this one does not, and a prefix that
   is used but not declared makes the whole view fail to parse — a blank screen,
   not a bad layout. An aggregation tag takes the namespace of *its own control*:
   under `<form:SimpleForm>` it is `<form:content>`, never `<content>`.

The frozen `z2ui5_cl_xml_view` is gone from here and must not come back. It sits
in the core's `src/99`, outside the released API, and the view gate can read
nothing built with it.

## Conventions

- ABAP classes are named ``z2ui5_cl_sel_*``; the variant table keeps its old name
  `z2ui5_t_13` (it holds data - a rename would drop it on every installation).
  No check enforces the prefix: `allowed_object_naming` in `abaplint.jsonc` only
  checks length and characters, and `.github/abaplint/rename.json` only proves
  that every `z2ui5_*` object still renames cleanly. Keep to it in review.
- English for code, comments, commit messages, pull requests and issues.
- All text files are LF-only (`.gitattributes`).
- The ecosystem-wide rules — workflow and npm-script naming, toolchain versions,
  which documentation files exist, commit style — live in
  [CONVENTIONS.md](https://github.com/abap2UI5/abap2UI5/blob/main/.github/shared/CONVENTIONS.md)
  and bind this repository too.
