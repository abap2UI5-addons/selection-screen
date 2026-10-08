# selection-screen

[![abap2UI5-addons](https://img.shields.io/badge/abap2UI5--addons-library-1873b4)](https://github.com/abap2UI5-addons)
[![ABAP](https://img.shields.io/badge/ABAP-Cloud%20%7C%20Standard%20%E2%89%A5%207.50-blue)](#installation)
[![abap2UI5](https://img.shields.io/badge/requires-abap2UI5-blue)](https://github.com/abap2UI5/abap2UI5)
[![License](https://img.shields.io/github/license/abap2UI5-addons/selection-screen)](LICENSE)
<br>
[![ABAP Cloud](https://img.shields.io/github/actions/workflow/status/abap2UI5-addons/selection-screen/abap-cloud.yaml?branch=main&label=ABAP%20Cloud)](https://github.com/abap2UI5-addons/selection-screen/actions/workflows/abap-cloud.yaml)
[![ABAP Standard](https://img.shields.io/github/actions/workflow/status/abap2UI5-addons/selection-screen/abap-standard.yaml?branch=main&label=ABAP%20Standard)](https://github.com/abap2UI5-addons/selection-screen/actions/workflows/abap-standard.yaml)
[![rename](https://img.shields.io/github/actions/workflow/status/abap2UI5-addons/selection-screen/check-rename.yaml?branch=main&label=rename)](https://github.com/abap2UI5-addons/selection-screen/actions/workflows/check-rename.yaml)
[![check-abap2UI5](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fabap2UI5-addons%2Fselection-screen%2Fbadges%2Fcheck-abap2ui5.json)](https://github.com/abap2UI5-addons/selection-screen/actions/workflows/check-abap2ui5.yaml)
[![abap2UI5](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fabap2UI5-addons%2Fselection-screen%2Fbadges%2Fabap2ui5.json)](https://github.com/abap2UI5-addons/selection-screen/actions/workflows/check-abap2ui5.yaml)

**Select-options with saved variants for your abap2UI5 apps - generated from a table name or your data.**
You pass a database table name or an internal table; selection-screen builds
one select-option per field, lets the user enter ranges in a popup, and saves
and loads the whole selection as a variant. Your app gets the filter back and
turns it into a SQL `WHERE` clause or filters an internal table with it. For
developers of abap2UI5 apps that read data on user-defined criteria.

> Part of [abap2UI5-addons](https://github.com/abap2UI5-addons) - addons and apps for [abap2UI5](https://github.com/abap2UI5/abap2UI5), installed with [abapGit](https://abapgit.org).

## Why

In SAP GUI a report gets its selection screen and variants for free with
`SELECT-OPTIONS`. In a UI5 app each filter field, its range dialog and the
variant handling would have to be built by hand.

selection-screen does that generically: it reads the fields via RTTI, renders
the select-options either inline in your page or as a filter popup, and keeps
variants in its own table `z2ui5_t_13` - per app, optionally per user, with a
default variant.

Good for:

- **Data browsers and reports** on abap2UI5 that select from a database table.
- **Filtering an internal table** the user already sees, with ranges instead
  of a search field.

## Installation

**Requirements**

- ABAP Cloud (S/4 Public Cloud, BTP ABAP Environment), S/4 Private Cloud or
  On-Premise, or SAP NetWeaver AS ABAP 7.50 or higher
- [abap2UI5](https://github.com/abap2UI5/abap2UI5)

**Steps** - with [abapGit](https://abapgit.org), in this order:

1. [abap2UI5](https://github.com/abap2UI5/abap2UI5)
2. this repository (branch `main`) - classes and the table `z2ui5_t_13` for
   the variants; nothing else to set up

**Start** - run a sample like any abap2UI5 app, e.g.
`?app_start=z2ui5_cl_sel_sample_01` for select-options over table `T100`.
All samples are listed under [Samples](#samples).

## Usage

Select-options for every field of a database table, inline in your page
(condensed from `z2ui5_cl_sel_sample_01`):

```abap
" DATA mo_multiselect TYPE REF TO z2ui5_cl_sel_multisel.

METHOD z2ui5_if_app~main.

  IF client->check_on_init( ).
    mo_multiselect = z2ui5_cl_sel_multisel=>factory_by_name( val       = `T100`
                                                             s_variant = VALUE #( handle01 = `ZMY_APP` ) ).
    view_display( ).    " calls mo_multiselect->set_output( client = client view = lo_panel )
    RETURN.
  ENDIF.

  " range popups, Clear, Load and Save variant are handled here
  IF mo_multiselect->main( client ).
    RETURN.
  ENDIF.

  IF client->check_on_event( `GO` ).
    DATA(lv_where) = z2ui5_cl_util=>filter_get_sql_where( mo_multiselect->ms_result-t_filter ).
    SELECT FROM t100 FIELDS * WHERE (lv_where) INTO TABLE @mt_t100 UP TO 100 ROWS.
    client->view_model_update( ).
  ENDIF.

ENDMETHOD.
```

As a filter popup over an internal table (condensed from
`z2ui5_cl_sel_sample_02`):

```abap
" on init
mo_variant = z2ui5_cl_sel_multisel_pop=>factory_by_data( data        = mt_table
                                                         var_handle1 = `ZMY_APP` ).
" on the Filter button
client->nav_app_call( mo_variant ).

" back in your app
IF client->check_on_navigated( ).
  DATA(lo_filter) = CAST z2ui5_cl_sel_multisel_pop( client->get_app_prev( ) ).
  IF lo_filter->result( )-check_confirmed = abap_true.
    z2ui5_cl_util=>filter_itab( EXPORTING filter = lo_filter->result( )-t_filter
                                CHANGING  val    = mt_table ).
  ENDIF.
ENDIF.
```

## Features

* Ranges, Filters, Selection-Screens
* Persist Variants

## What is there

| Class | Purpose |
|---|---|
| `z2ui5_cl_sel_multisel` | select-options for a table name (`factory_by_name`), for data (`factory_by_data`) or for a filter (`factory_by_filter`); renders into your view with `set_output( )`, handles its events in `main( )` |
| `z2ui5_cl_sel_multisel_pop` | the same as a popup app; returns the filter with `result( )` |
| `z2ui5_cl_sel_var_pop_read`, `z2ui5_cl_sel_var_pop_save` | popups to load and save a variant |
| `z2ui5_cl_sel_var_db` | variant persistence in table `z2ui5_t_13` |
| `z2ui5_cl_sel_screen`, `z2ui5_cl_sel_screen_sel` | skeleton of a `PARAMETERS`-style selection screen - the methods are still empty |

For a full report-style selection screen with `PARAMETERS`, `SELECT-OPTIONS`
and event blocks, see [abap2UI5-addons/abap-cloud-gui](https://github.com/abap2UI5-addons/abap-cloud-gui).

## Samples

| Sample | Shows |
|---|---|
| `z2ui5_cl_sel_sample_01` | Select-options for table `T100` inline in the page, result read with a dynamic `WHERE` |
| `z2ui5_cl_sel_sample_02` | Filter popup over an internal table |
| `z2ui5_cl_sel_sample_03` | Skeleton of the `PARAMETERS`-style selection screen |

## Demo

### Selection-Screen
<img width="600" alt="image" src="https://github.com/user-attachments/assets/47eb179e-c563-402d-907f-58ac77b43941" />

### Filter Popup
<img width="600" alt="Google Chrome 2025-05-03 10 38 12" src="https://github.com/user-attachments/assets/f5c76993-9ad4-46a0-81df-2586ec2c21cf" />
<img width="600" alt="image" src="https://github.com/user-attachments/assets/84c4fa61-a95a-4232-b7c1-0f9d3105e20d" />

## Development

```bash
npm ci
npm run check           # all gates below, as in CI
npm run lint            # abaplint, Standard ABAP (v750)
npm run check:cloud     # abaplint, ABAP Cloud
npm run check:abap2ui5  # abap2UI5-linter over apps and views
npm run rename          # namespace rename check
```

## Contributing

Issues and pull requests are welcome - whether you're fixing bugs, adding new
functionality, or improving documentation. Read
[CONTRIBUTING.md](CONTRIBUTING.md) first.

## License

MIT - see [LICENSE](LICENSE).
